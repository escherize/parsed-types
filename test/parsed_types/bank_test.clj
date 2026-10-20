(ns parsed-types.bank-test
  "The README's examples, run: a transfer between accounts, a payroll of transfers parsed in one call, and a budget
  whose parts are budgets."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

;;; An in-memory bank.

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

(pt/defparsed Transfer [user-id from-id to-id amount]
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
  (external ::source-exists "The source account exists."
            [] [source ::source]
            (some? source))
  (external ::destination-exists "The destination account exists."
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
  (let [{:keys [from-id to-id amount]} (pt/parsed Transfer transfer)]
    (swap! accounts #(-> %
                         (update-in [from-id :balance] - amount)
                         (update-in [to-id :balance] + amount)))))

(defn- thrown-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest transfer-test
  (testing "a valid transfer parses, and the sink moves the money"
    (let [t (pt/parse (pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 2, :amount 30M}))]
      (is (= {:user-id :ada, :from-id 1, :to-id 2, :amount 30M} (into {} t)))
      (transfer! t)
      (is (= [70M 40M] (map #(get-in @accounts [% :balance]) [1 2])))))
  (testing "malformed input fails pure checks, reports every failure, and fetches nothing"
    (reset! fetched [])
    (is (= {:failed-kinds #{:pure}
            :failures    {::amount-positive {:doc      "The amount is a positive decimal."
                                             :returned false
                                             :blocked  [::source-exists ::destination-exists ::owns-source
                                                        ::funds-available]}
                          ::two-accounts    {:doc      "Money moves between two different accounts."
                                             :returned false
                                             :blocked  [::source-exists ::destination-exists ::owns-source
                                                        ::funds-available]}}}
           (select-keys (thrown-data #(pt/parse (->TransferDraft :ada 1 1 30))) [:failed-kinds :failures])))
    (is (= [] @fetched)))
  (testing "a missing account, and someone else's account, fail external checks"
    (let [missing (thrown-data #(pt/parse (->TransferDraft :ada 9 2 1M)))]
      (is (= #{:external} (:failed-kinds missing)))
      (is (contains? (:failures missing) ::source-exists)))
    (is (= {:failed-kinds #{:external}
            :failures     {::owns-source {:doc "The user owns the source account.", :returned false}}}
           (select-keys (thrown-data #(pt/parse (->TransferDraft :bob 1 2 1M))) [:failed-kinds :failures]))))
  (testing "more than the balance fails an external check (bob's account holds 40M after the first transfer)"
    (is (= {::funds-available {:doc "The source account holds the amount.", :returned false}}
           (:failures (thrown-data #(pt/parse (->TransferDraft :bob 2 1 41M)))))))
  (testing "the sink refuses a transfer built by hand"
    (is (= ::pt/not-parsed (:type (thrown-data #(transfer! (Transfer. :ada 1 2 1000M nil))))))
    (is (= ::pt/not-parsed (:type (thrown-data #(transfer! (->TransferDraft :ada 1 2 1000M))))))))

;;; A payroll: one account pays several people, parsed with its transfers in one call

(pt/defparsed Payroll [user-id from-id transfers]
  (children transfers Transfer)
  (pure ::some-transfers "A payroll pays someone."
        [transfers]
        (boolean (seq transfers)))
  (pure ::no-repeat-payee "No account is paid twice."
        [transfers]
        (= (count transfers) (count (set (map :to-id transfers)))))
  (pure ::one-source "Every transfer draws on the payroll's account."
        [from-id transfers]
        (every? #(= from-id (:from-id %)) transfers))
  (pure ::one-payer "Every transfer is made by the payroll's user."
        [user-id transfers]
        (every? #(= user-id (:user-id %)) transfers))
  (external ::covers-total "The account holds the sum of every transfer."
            [transfers] [source ::source]
            (<= (reduce + 0M (map :amount transfers)) (:balance source 0M)))
  (fetch ::source [from-id] accounts-by-id))

(defn- run-payroll!
  "Pay everyone. Each child is already a parsed Transfer, so it goes straight to transfer!."
  [payroll]
  (let [{:keys [transfers]} (pt/parsed Payroll payroll)]
    (run! transfer! transfers)))

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
    (is (= {:failed-kinds #{:pure}
            :failures    {:transfers         {1 {::amount-positive {:doc      "The amount is a positive decimal."
                                                                    :returned false
                                                                    :blocked  [::source-exists ::destination-exists
                                                                               ::owns-source ::funds-available]}}}
                          ;; held: it reads transfers, and transfer 1 failed; it holds every later check that
                          ;; reads transfers
                          ::some-transfers   {:doc        "A payroll pays someone."
                                              :blocked-by [[:transfers 1]]
                                              :blocked    [::no-repeat-payee ::one-source ::one-payer
                                                           ::covers-total]}}}
           (select-keys (thrown-data #(pt/parse (payroll :ada 1  2 10M  3 -5M))) [:failed-kinds :failures])))
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
        edited (assoc-in draft [:transfers 1] (->TransferDraft :ada 1 3 25M))]
    (testing "draft-of goes back down the tree: the children are drafts again"
      (is (every? #(instance? TransferDraft %) (:transfers draft))))
    (is (= 25M (:amount (second (:transfers (pt/parse edited))))))))

(deftest a-mutable-value-in-a-child-is-refused-test
  (testing "every draft in the tree is screened, and the tree fetches nothing"
    (is (= {:type ::pt/bad-input, :draft "parsed_types.bank_test.TransferDraft", :field :amount}
           (select-keys (thrown-data #(pt/parse (->PayrollDraft :ada 1 [(->TransferDraft :ada 1 2 (atom 1M))])))
                        [:type :draft :field])))
    (is (= [] @fetched)))
  (testing "a child field's vector may carry no metadata, empty or not, since the parsed value holds a new vector"
    (doseq [kids [[] [(->TransferDraft :ada 1 2 1M)]]]
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/child-field-metadata, :field :transfers}
             (select-keys (thrown-data #(pt/parse (->PayrollDraft :ada 1 (with-meta kids {:sneaky (atom 1)}))))
                          [:type :rule :field]))))))

;;; A budget: a tree of one type

(pt/defparsed Budget [name amount parts]
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

;;; Grandchildren, and the limits on a tree's size

(pt/defparsed Leaf [amount]
  (pure ::amount-int "The amount is an integer." [amount] (int? amount)))

(pt/defparsed Middle [leaf]
  (child leaf Leaf))

(pt/defparsed Top [middles]
  (children middles Middle)
  (pure ::leaves-positive "Every leaf's amount is positive."
        [middles]
        (every? #(pos? (:amount (:leaf %))) middles)))

(deftest a-failed-grandchild-holds-the-grandparent-test
  (testing "the middle has no check of its own that reads the leaf, and the top's check is still held"
    (is (= {:failed-kinds #{:pure}
            :failures    {:middles            {0 {:leaf {::amount-int {:doc "The amount is an integer.", :returned false}}}}
                          ::leaves-positive   {:doc "Every leaf's amount is positive.", :blocked-by [[:middles 0]]}}}
           (select-keys (thrown-data #(pt/parse (->TopDraft [(->MiddleDraft (->LeafDraft "x"))]))) [:failed-kinds :failures])))))

(pt/defparsed Chain [next]
  (child next Chain))

(deftest tree-limits-test
  (testing "a tree deeper than the limit is bad input, not a stack overflow"
    (is (= ::pt/bad-input (:type (thrown-data #(pt/parse (reduce (fn [c _] (->ChainDraft c)) (->ChainDraft nil) (range 1500))))))))
  (testing "a deep tree within the limit parses"
    (is (pt/parsed? Chain (pt/parse (reduce (fn [c _] (->ChainDraft c)) (->ChainDraft nil) (range 900))))))
  (testing "a draft shared at every level counts each time it appears, so the tree has a size limit"
    (is (= ::pt/bad-input (:type (thrown-data #(pt/parse (reduce (fn [b _] (->BudgetDraft "b" 0M [b b])) (->BudgetDraft "b" 0M []) (range 20)))))))))

;;; A sum: a payment is a transfer or a payroll

(pt/defsum Payment [Transfer Payroll])

(defn- pay!
  "Make any payment. match must handle every member of Payment, or this does not compile."
  [payment]
  (pt/match Payment payment
    Transfer t (transfer! t)
    Payroll  p (run-payroll! p)))

(pt/defparsed Batch [payments]
  (children payments Payment)
  (pure ::some-payments "A batch makes some payment."
        [payments]
        (boolean (seq payments))))

(deftest sum-test
  (testing "match runs the branch for the member, with the value as that type"
    (pay! (pt/parse (->TransferDraft :ada 1 2 5M)))
    (pay! (pt/parse (payroll :ada 1  2 5M  3 5M)))
    (is (= [85M 20M 5M] (map #(get-in @accounts [% :balance]) [1 2 3]))))
  (testing "match refuses anything parse did not issue, member type or not"
    (is (= ::pt/not-parsed (:type (thrown-data #(pay! (Transfer. :ada 1 2 1000M nil))))))
    (is (= ::pt/not-parsed (:type (thrown-data #(pay! (->TransferDraft :ada 1 2 1M))))))
    (is (= ::pt/not-parsed (:type (thrown-data #(pay! (pt/parse (budget "b" 1M)))))))
    (is (= [85M 20M 5M] (map #(get-in @accounts [% :balance]) [1 2 3])) "nothing moved"))
  (testing "parsed and parsed? take a sum"
    (is (pt/parsed? Payment (pt/parse (->TransferDraft :ada 1 2 1M))))
    (is (not (pt/parsed? Payment (pt/parse (budget "b" 1M)))))))

(deftest sum-children-test
  (testing "a child field of a sum type holds drafts of any member, parsed with the parent"
    (let [b (pt/parse (->BatchDraft [(->TransferDraft :ada 1 2 1M) (payroll :ada 1  3 1M)]))]
      (is (= [Transfer Payroll] (map class (:payments b))))
      (is (every? #(pt/parsed? Payment %) (:payments b)))
      (run! pay! (:payments b))))
  (testing "a draft of a type outside the sum fails the shape check"
    (is (= {:parsed-types.child/payments {:doc      "payments holds a vector of drafts that parse to Payment."
                                          :returned false
                                          :blocked  [::some-payments]}}
           (:failures (thrown-data #(pt/parse (->BatchDraft [(budget "b" 1M)]))))))))

(deftest depth-boundary-test
  (let [chain (fn [levels] (reduce (fn [c _] (->ChainDraft c)) (->ChainDraft nil) (range (dec levels))))]
    (is (pt/parsed? Chain (pt/parse (chain 1000))) "1000 levels, counting the root, parse")
    (is (= ::pt/bad-input (:type (thrown-data #(pt/parse (chain 1001))))) "1001 do not")))

;;; What a parent check sees of its children

(pt/defparsed Watcher [transfers]
  (children transfers Transfer)
  (pure ::children-unproven "Inside a check, no child is parsed yet."
        [transfers]
        (not-any? #(pt/parsed? Transfer %) transfers)))

(deftest a-check-sees-children-without-the-proof-test
  (let [w (pt/parse (->WatcherDraft [(->TransferDraft :ada 1 2 1M) (->TransferDraft :ada 1 3 1M)]))]
    (testing "the check passed, so pt/parsed refused every child while it ran"
      (is (pt/parsed? Watcher w)))
    (testing "once the tree passed, the issued root holds issued children"
      (is (every? #(pt/parsed? Transfer %) (:transfers w))))))

;;; A parent check that misspells a child's field

(pt/defparsed Sloppy [transfers]
  (children transfers Transfer)
  (pure ::none-frozen "No transfer is frozen." [transfers] (not-any? :frozen? transfers)))

(deftest a-parent-check-cannot-read-a-key-a-child-lacks-test
  (testing "children are closed values inside parent checks too: a misspelled key throws, never nil"
    (is (= ::pt/no-field (:type (thrown-data #(pt/parse (->SloppyDraft [(->TransferDraft :ada 1 2 1M)]))))))))

;;; A fetch that leaves out a tuple

(defn- forgetful-rows [tuples] (into {} (map (fn [t] [t {:ok true}])) (rest tuples)))

(pt/defparsed Forgot [id]
  (external ::row-ok "The row is ok." [] [row ::row] (true? (:ok row)))
  (fetch ::row [id] forgetful-rows))

(deftest a-fetch-must-answer-every-tuple-test
  (testing "a missing tuple throws, never a nil row a check could pass on"
    (is (= ::pt/bad-fetch (:type (thrown-data #(pt/parse-all [(->ForgotDraft 1) (->ForgotDraft 2)])))))
    (is (= (str "Fetch :parsed-types.bank-test/row returned no entry for [[1]]. A fetch answers every tuple it is given,"
                " so no check reads a nil it did not mean. Return a key for every tuple, with nil for not found, as in"
                " (into {} (map (fn [t] [t (lookup t)])) tuples).")
           (try (pt/parse-all [(->ForgotDraft 1) (->ForgotDraft 2)]) nil
                (catch clojure.lang.ExceptionInfo e (ex-message e)))))))

;;; A child's external failure holds the parent's external checks over it

(pt/defparsed Grumpy [transfers]
  (children transfers Transfer)
  (external ::boom "Throws whenever it runs." [transfers] (throw (ex-info "boom" {:transfers (count transfers)}))))

(deftest a-child-external-failure-holds-the-parent-test
  (testing "bob may not draw on account 1: the payroll's check over transfers waits, and is held"
    (is (= {:failed-kinds #{:external}
            :failures    {:transfers     {0 {::owns-source {:doc "The user owns the source account.", :returned false}}}
                          ::covers-total {:doc "The account holds the sum of every transfer.", :blocked-by [[:transfers 0]]}}}
           (select-keys (thrown-data #(pt/parse (payroll :bob 1  2 10M))) [:failed-kinds :failures]))))
  (testing "a parent check that would throw never runs, so the child's failure is what you get"
    (is (= #{:external} (:failed-kinds (thrown-data #(pt/parse (->GrumpyDraft [(->TransferDraft :bob 1 2 10M)]))))))))

(deftest node-limit-boundary-test
  (let [wide (fn [n] (->BudgetDraft "root" 1000000M (vec (repeatedly (dec n) #(->BudgetDraft "p" 0M [])))))]
    (is (pt/parsed? Budget (pt/parse (wide 100000))) "100,000 drafts parse")
    (is (= ::pt/bad-input (:type (thrown-data #(pt/parse (wide 100001))))) "100,001 do not")))
