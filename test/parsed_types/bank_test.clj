(ns parsed-types.bank-test
  "The README's examples, run: a transfer between accounts, a payroll of transfers parsed in one call, and a budget
  whose parts are budgets."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

;;; An in-memory bank. A check may not close over a local, so the bank and the fetch log are vars.

(def ^:private accounts (atom {}))

(def ^:private fetched (atom []))

(defn- accounts-by-id
  "`{[id] account}` for each `[id]` in `arg-tuples`, nil for an id with no account: one lookup for the whole call."
  [arg-tuples]
  (swap! fetched conj (mapv first arg-tuples))
  (into {} (map (fn [[id :as t]] [t (get @accounts id)])) arg-tuples))

(use-fixtures :each (fn [t]
                      (reset! accounts {1 {:owner :ada, :balance 100M}
                                        2 {:owner :bob, :balance 10M}
                                        3 {:owner :cy,  :balance 0M}})
                      (reset! fetched [])
                      (t)))

;;; A transfer

(pt/defdraft TransferDraft -> Transfer [user-id from-id to-id amount]
  (pure ::amount-positive "The amount is a positive decimal."
        [amount]
        (and (decimal? amount) (pos? amount)))
  (pure ::from-id-positive "The source account id is a positive integer."
        [from-id]
        (pos-int? from-id))
  (pure ::to-id-positive "The destination account id is a positive integer."
        [to-id]
        (pos-int? to-id))
  (pure ::two-accounts "Money moves between two different accounts."
        [from-id to-id]
        (not= from-id to-id))
  (external ::source-exists "The source account exists." {:status 404}
            [] [source ::source]
            (some? source))
  (external ::destination-exists "The destination account exists." {:status 404}
            [] [destination ::destination]
            (some? destination))
  (external ::owns-source "The user owns the source account."
            [user-id] [source ::source]
            (= user-id (:owner source)))
  (external ::funds-available "The source account holds the amount."
            [amount] [source ::source]
            (<= amount (:balance source 0M)))
  (fetch ::source [from-id] accounts-by-id)
  (fetch ::destination [to-id] accounts-by-id))

(defn- transfer!
  "Move the money. Takes a parsed Transfer and nothing else."
  [transfer]
  (let [^Transfer t (pt/parsed Transfer transfer)]
    (swap! accounts #(-> %
                         (update-in [(.-from-id t) :balance] - (.-amount t))
                         (update-in [(.-to-id t) :balance] + (.-amount t))))))

(defn- thrown-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest transfer-test
  (testing "a valid transfer parses, and the sink moves the money"
    (let [t (pt/parse (->TransferDraft :ada 1 2 30M))]
      (is (= {:user-id :ada, :from-id 1, :to-id 2, :amount 30M} t))
      (transfer! t)
      (is (= [70M 40M] (map #(get-in @accounts [% :balance]) [1 2])))))
  (testing "malformed input is a 400 with every failure, and nothing is fetched"
    (reset! fetched [])
    (is (= {:status-code 400
            :failures    {::amount-positive {:doc      "The amount is a positive decimal."
                                             :returned false
                                             :blocked  [::source-exists ::destination-exists ::owns-source
                                                        ::funds-available]}
                          ::two-accounts    {:doc      "Money moves between two different accounts."
                                             :returned false
                                             :blocked  [::source-exists ::destination-exists ::owns-source
                                                        ::funds-available]}}}
           (select-keys (thrown-data #(pt/parse (->TransferDraft :ada 1 1 30))) [:status-code :failures])))
    (is (= [] @fetched)))
  (testing "a missing account is a 404, and someone else's account a 403"
    (is (= 404 (:status-code (thrown-data #(pt/parse (->TransferDraft :ada 9 2 1M))))))
    (is (= 403 (:status-code (thrown-data #(pt/parse (->TransferDraft :bob 1 2 1M)))))))
  (testing "more than the balance is a 403 (bob's account holds 40M after the first transfer)"
    (is (= {::funds-available {:doc "The source account holds the amount.", :returned false}}
           (:failures (thrown-data #(pt/parse (->TransferDraft :bob 2 1 41M)))))))
  (testing "the sink refuses a transfer built by hand"
    (is (= 500 (:status-code (thrown-data #(transfer! (->Transfer :ada 1 2 1000M))))))
    (is (= 500 (:status-code (thrown-data #(transfer! (->TransferDraft :ada 1 2 1000M))))))))

;;; A payroll: one account pays several people, parsed with its transfers in one call

(pt/defdraft PayrollDraft -> Payroll [user-id from-id transfers]
  (children transfers Transfer)
  (pure ::some-transfers "A payroll pays someone."
        [transfers]
        (boolean (seq transfers)))
  (pure ::one-source "Every transfer draws on the payroll's account."
        [from-id transfers]
        (every? #(= from-id (:from-id %)) transfers))
  (pure ::one-payer "Every transfer is made by the payroll's user."
        [user-id transfers]
        (every? #(= user-id (:user-id %)) transfers))
  (pure ::no-repeat-payee "No account is paid twice."
        [transfers]
        (= (count transfers) (count (set (map :to-id transfers)))))
  (external ::covers-total "The account holds the sum of every transfer."
            [transfers] [source ::source]
            (<= (reduce + 0M (map :amount transfers)) (:balance source 0M)))
  (fetch ::source [from-id] accounts-by-id))

(defn- run-payroll!
  "Pay everyone. Each child is already a parsed Transfer, so it goes straight to transfer!."
  [payroll]
  (let [^Payroll p (pt/parsed Payroll payroll)]
    (run! transfer! (.-transfers p))))

(defn- payroll [& [user-id from-id & pays]]
  (->PayrollDraft user-id from-id (mapv (fn [[to amount]] (->TransferDraft user-id from-id to amount))
                                        (partition 2 pays))))

(deftest payroll-test
  (testing "the payroll and its transfers parse in one call, with one fetch per declaration and fetch"
    (let [p (pt/parse (payroll :ada 1  2 10M  3 20M))]
      (is (every? #(pt/parsed? Transfer %) (:transfers p)))
      (is (= #{[1] [2 3]} (set @fetched)) "the transfers' sources, their destinations, and the payroll's source")
      (is (= 3 (count @fetched)))
      (run-payroll! p)
      (is (= [70M 20M 20M] (map #(get-in @accounts [% :balance]) [1 2 3])))))
  (testing "a malformed transfer fails the payroll, under its index, and nothing in the tree is fetched"
    (reset! fetched [])
    (is (= {:status-code 400
            :failures    {:transfers         {1 {::amount-positive {:doc      "The amount is a positive decimal."
                                                                    :returned false
                                                                    :blocked  [::source-exists ::destination-exists
                                                                               ::owns-source ::funds-available]}}}
                          ;; held: they read transfers, and transfer 1 failed
                          ::some-transfers   {:doc        "A payroll pays someone."
                                              :blocked-by [[:transfers 1]]
                                              :blocked    [::one-source ::one-payer ::covers-total]}
                          ::no-repeat-payee  {:doc        "No account is paid twice."
                                              :blocked-by [[:transfers 1]]
                                              :blocked    [::one-source ::one-payer ::covers-total]}}}
           (select-keys (thrown-data #(pt/parse (payroll :ada 1  2 10M  3 -5M))) [:status-code :failures])))
    (is (= [] @fetched)))
  (testing "each transfer fits the balance, but together they don't: only the parent can see that"
    (is (= {::covers-total {:doc "The account holds the sum of every transfer.", :returned false}}
           (:failures (thrown-data #(pt/parse (payroll :ada 1  2 60M  3 60M)))))))
  (testing "a payroll holds drafts: a transfer parsed elsewhere, perhaps for someone else, is refused"
    (let [elsewhere (pt/parse (->TransferDraft :ada 1 2 1M))]
      (is (= {:parsed-types.child/transfers {:doc      "transfers holds a vector of drafts that parse to Transfer."
                                             :returned false
                                             :blocked  [::some-transfers ::no-repeat-payee ::one-source ::one-payer
                                                        ::covers-total]}}
             (:failures (thrown-data #(pt/parse (->PayrollDraft :ada 1 [elsewhere])))))))))

(deftest edit-a-parsed-payroll-test
  (let [p      (pt/parse (payroll :ada 1  2 10M  3 20M))
        draft  (pt/draft-of p)
        edited (pt/assoc-field draft :transfers (assoc (:transfers draft) 1 (->TransferDraft :ada 1 3 25M)))]
    (testing "draft-of goes back down the tree: the children are drafts again, known to pass"
      (is (every? #(instance? TransferDraft %) (:transfers draft)))
      (is (contains? (pt/passed-checks (first (:transfers draft))) ::amount-positive)))
    (testing "after editing one transfer, the parent's checks over transfers forget their passes"
      (is (not (contains? (pt/passed-checks edited) ::one-source))))
    (is (= 25M (:amount (second (:transfers (pt/parse edited))))))))

;;; A budget: a tree of one type

(pt/defdraft BudgetDraft -> Budget [name amount parts]
  (children parts Budget)
  (pure ::name-not-blank "The name is a non-blank string."
        [name]
        (and (string? name) (not (str/blank? name))))
  (pure ::amount-not-negative "The amount is a decimal of at least zero."
        [amount]
        (and (decimal? amount) (not (neg? amount))))
  (pure ::parts-fit "The parts add up to at most the amount."
        [amount parts]
        (<= (reduce + 0M (map :amount parts)) amount)))

(defn- budget [name amount & parts]
  (->BudgetDraft name amount (vec parts)))

(deftest budget-test
  (testing "a parsed root proves the whole tree"
    (let [b (pt/parse (budget "company" 100M
                              (budget "eng" 60M (budget "infra" 20M) (budget "tools" 30M))
                              (budget "sales" 40M)))]
      (is (= "tools" (get-in b [:parts 0 :parts 1 :name])))
      (is (every? #(pt/parsed? Budget %) (tree-seq map? :parts b)))))
  (testing "a failure deep in the tree is reported at its path, and the checks above it that read it are held"
    (is (= {:parts {0 {:parts               {1 {::amount-not-negative {:doc      "The amount is a decimal of at least zero."
                                                                       :returned false
                                                                       :blocked  [::parts-fit]}}}
                       ::parts-fit          {:doc "The parts add up to at most the amount.", :blocked-by [[:parts 0 :parts 1]]}}}
            ::parts-fit {:doc "The parts add up to at most the amount.", :blocked-by [[:parts 0]]}}
           (:failures (thrown-data #(pt/parse (budget "company" 100M
                                                      (budget "eng" 60M (budget "infra" 20M) (budget "tools" -1M)))))))))
  (testing "parts that overspend their parent fail at the parent"
    (is (= {:parts {0 {::parts-fit {:doc "The parts add up to at most the amount.", :returned false}}}
            ::parts-fit {:doc "The parts add up to at most the amount.", :blocked-by [[:parts 0]]}}
           (:failures (thrown-data #(pt/parse (budget "company" 100M
                                                      (budget "eng" 60M (budget "infra" 50M) (budget "tools" 30M))))))))))

(deftest check-on-a-subset-does-not-touch-the-world-test
  (testing "checking some of a payroll's pure checks leaves the rest unproven, so nothing in the tree fetches"
    (is (true? (pt/check (payroll :ada 1  2 10M) [::no-repeat-payee])))
    (is (= [] @fetched))))

;;; Grandchildren, and the limits on a tree's size

(pt/defdraft LeafDraft -> Leaf [amount]
  (pure ::amount-int "The amount is an integer." [amount] (int? amount)))

(pt/defdraft MiddleDraft -> Middle [leaf]
  (child leaf Leaf))

(pt/defdraft TopDraft -> Top [middles]
  (children middles Middle)
  (pure ::leaves-positive "Every leaf's amount is positive."
        [middles]
        (every? #(pos? (:amount (:leaf %))) middles)))

(deftest a-failed-grandchild-holds-the-grandparent-test
  (testing "the middle has no check of its own that reads the leaf, and the top's check is still held"
    (is (= {:status-code 400
            :failures    {:middles            {0 {:leaf {::amount-int {:doc "The amount is an integer.", :returned false}}}}
                          ::leaves-positive   {:doc "Every leaf's amount is positive.", :blocked-by [[:middles 0]]}}}
           (select-keys (thrown-data #(pt/parse (->TopDraft [(->MiddleDraft (->LeafDraft "x"))]))) [:status-code :failures])))))

(pt/defdraft ChainDraft -> Chain [next]
  (child next Chain))

(deftest tree-limits-test
  (testing "a tree deeper than the limit is a 400, not a stack overflow"
    (is (= 400 (:status-code (thrown-data #(pt/parse (reduce (fn [c _] (->ChainDraft c)) (->ChainDraft nil) (range 1500))))))))
  (testing "a deep tree within the limit parses"
    (is (pt/parsed? Chain (pt/parse (reduce (fn [c _] (->ChainDraft c)) (->ChainDraft nil) (range 900))))))
  (testing "a draft shared at every level counts each time it appears, so the tree has a size limit"
    (is (= 400 (:status-code (thrown-data #(pt/parse (reduce (fn [b _] (->BudgetDraft "b" 0M [b b])) (->BudgetDraft "b" 0M []) (range 20)))))))))
