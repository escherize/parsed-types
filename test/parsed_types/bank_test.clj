(ns parsed-types.bank-test
  "A bank: a transfer between accounts, and a payroll that holds transfers parsed together in one call."
  (:require
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
      (is (= {:user-id :ada, :from-id 1, :to-id 2, :amount 30M} (pt/parsed Transfer t)))
      (transfer! t)
      (is (= [70M 40M] (map #(get-in @accounts [% :balance]) [1 2])))))
  (testing "malformed input fails the first pure check that refuses it, and fetches nothing"
    (reset! fetched [])
    (is (= {:kind :pure, :check ::amount-positive, :returned false}
           (select-keys (thrown-data #(pt/parse (pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 1, :amount 30}))) [:kind :check :returned])))
    (is (= [] @fetched)))
  (testing "a missing account fails the existence check, and the message says the fetch found nothing"
    (is (= {:kind :external, :check ::source-exists}
           (select-keys (thrown-data #(pt/parse (pt/draft Transfer {:user-id :ada, :from-id 9, :to-id 2, :amount 1M}))) [:kind :check])))
    (is (re-find #"source-exists \(.*?\) failed, and its fetch :parsed-types.bank-test/source found nothing"
                 (try (pt/parse (pt/draft Transfer {:user-id :ada, :from-id 9, :to-id 2, :amount 1M})) nil
                      (catch clojure.lang.ExceptionInfo e (ex-message e))))))
  (testing "someone else's account fails the permission check, and nothing after it runs"
    ;; so the balance of an account the user may not see is never checked, and never reported
    (is (= {:kind :external, :check ::owns-source, :returned false}
           (select-keys (thrown-data #(pt/parse (pt/draft Transfer {:user-id :bob, :from-id 1, :to-id 2, :amount 1000M}))) [:kind :check :returned]))))
  (testing "more than the balance fails an external check (bob's account holds 40M after the first transfer)"
    (is (= ::funds-available (:check (thrown-data #(pt/parse (pt/draft Transfer {:user-id :bob, :from-id 2, :to-id 1, :amount 41M})))))))
  (testing "the sink refuses a transfer built by hand"
    (is (= ::pt/not-parsed (:type (thrown-data #(transfer! (Transfer. :ada 1 2 1000M nil))))))
    (is (= ::pt/not-parsed (:type (thrown-data #(transfer! (pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 2, :amount 1000M}))))))))

;;; A payroll: one account pays several people. Its transfers are parsed first, together, and the payroll holds them

(pt/defparsed Payroll [user-id from-id transfers]
  (pure ::transfers-parsed "Every transfer is a parsed Transfer."
        [transfers]
        (and (vector? transfers) (every? #(pt/parsed? Transfer %) transfers)))
  (pure ::some-transfers "A payroll pays someone."
        [transfers]
        (boolean (seq transfers)))
  (pure ::no-repeat-payee "No account is paid twice."
        [transfers]
        (= (count transfers) (count (set (map #(:to-id (pt/parsed Transfer %)) transfers)))))
  (pure ::one-source "Every transfer draws on the payroll's account."
        [from-id transfers]
        (every? #(= from-id (:from-id (pt/parsed Transfer %))) transfers))
  (pure ::one-payer "Every transfer is made by the payroll's user."
        [user-id transfers]
        (every? #(= user-id (:user-id (pt/parsed Transfer %))) transfers))
  (external ::covers-total "The account holds the sum of every transfer."
            [transfers] [source ::source]
            (<= (reduce + 0M (map #(:amount (pt/parsed Transfer %)) transfers)) (:balance source 0M)))
  (fetch ::source [from-id] accounts-by-id))

(defn- run-payroll!
  "Pay everyone. Each transfer is already a parsed Transfer, so it goes straight to transfer!."
  [payroll]
  (let [{:keys [transfers]} (pt/parsed Payroll payroll)]
    (run! transfer! transfers)))

(defn- payroll
  "The draft of a payroll, its transfers parsed first in one call."
  [& [user-id from-id & pays]]
  (pt/draft Payroll {:user-id user-id, :from-id from-id, :transfers (pt/parse-all (mapv (fn [[to amount]] (pt/draft Transfer {:user-id user-id, :from-id from-id, :to-id to, :amount amount}))
                                                      (partition 2 pays)))}))

(deftest payroll-test
  (testing "the transfers parse in one call, then the payroll, with one fetch per declaration and fetch"
    (let [p (pt/parse (payroll :ada 1  2 10M  3 20M))]
      (is (every? #(pt/parsed? Transfer %) (:transfers (pt/parsed Payroll p))))
      (is (= [[1] [2 3] [1]] @fetched) "the transfers' sources, their destinations, then the payroll's source")
      (run-payroll! p)
      (is (= [70M 20M 20M] (map #(get-in @accounts [% :balance]) [1 2 3])))))
  (testing "a malformed transfer fails parse-all with its index, and nothing fetches"
    (reset! fetched [])
    (is (= {:kind :pure, :check ::amount-positive, :index 1}
           (select-keys (thrown-data #(payroll :ada 1  2 10M  3 -5M)) [:kind :check :index])))
    (is (= [] @fetched)))
  (testing "each transfer fits the balance, but together they don't: only the payroll can see that"
    (is (= ::covers-total (:check (thrown-data #(pt/parse (payroll :ada 1  2 60M  3 60M)))))))
  (testing "a payroll of 50 transfers, parsed with parse-all, fetches once per fetch: no call per transfer"
    (reset! accounts (into {1 {:owner :ada, :balance 1000M}} (for [i (range 2 52)] [i {:owner :bob, :balance 0M}])))
    (reset! fetched [])
    (let [p (pt/parse (apply payroll :ada 1 (mapcat (fn [i] [i 1M]) (range 2 52))))]
      (is (= 50 (count (:transfers (pt/parsed Payroll p)))))
      (is (= [[1] (vec (range 2 52)) [1]] @fetched)))))

(deftest a-field-holds-parsed-values-not-drafts-test
  (testing "a draft in a field is refused, because no check of this parse looks inside it"
    (let [data (thrown-data #(pt/parse (pt/draft Payroll {:user-id :ada, :from-id 1, :transfers [(pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 2, :amount 1M})]})))]
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/draft-in-field, :field :transfers, :path [0]
              :got "parsed_types.bank_test.TransferDraft"}
             (select-keys data [:type :rule :field :path :got]))))
    (is (= [] @fetched) "and nothing is fetched")
    (testing "and the fix the message names parses"
      (is (pt/parsed? Payroll (pt/parse (pt/draft Payroll {:user-id :ada, :from-id 1, :transfers (pt/parse-all [(pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 2, :amount 1M})])}))))))
  (testing "a value of a parsed type that parse did not issue is refused too"
    (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/unissued-in-field, :field :transfers, :path [0]}
           (select-keys (thrown-data #(pt/parse (pt/draft Payroll {:user-id :ada, :from-id 1, :transfers [(Transfer. :ada 1 2 1M nil)]})))
                        [:type :rule :field :path])))))

(deftest edit-a-parsed-payroll-test
  (let [p      (pt/parse (payroll :ada 1  2 10M  3 20M))
        draft  (pt/draft-of p)
        edited (assoc draft :transfers (assoc (:transfers (pt/parsed Payroll p)) 1 (pt/parse (pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 3, :amount 25M}))))]
    (testing "draft-of returns a draft of the payroll, which still holds the parsed transfers"
      (is (= p (pt/parse draft))))
    (is (= 25M (:amount (pt/parsed Transfer (second (:transfers (pt/parsed Payroll (pt/parse edited))))))))))

;;; A sum: a payment is a transfer or a payroll

(pt/defsum Payment [Transfer Payroll])

(pt/defparsed Memo [text])

(defn- pay!
  "Make any payment. match must handle every member of Payment, or this does not compile."
  [payment]
  (pt/match Payment payment
    Transfer t (transfer! t)
    Payroll  p (run-payroll! p)))

(deftest sum-test
  (testing "match runs the branch for the member, with the value as that type"
    (pay! (pt/parse (pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 2, :amount 5M})))
    (pay! (pt/parse (payroll :ada 1  2 5M  3 5M)))
    (is (= [85M 20M 5M] (map #(get-in @accounts [% :balance]) [1 2 3]))))
  (testing "match refuses anything parse did not issue, member type or not"
    (is (= ::pt/not-parsed (:type (thrown-data #(pay! (Transfer. :ada 1 2 1000M nil))))))
    (is (= ::pt/not-parsed (:type (thrown-data #(pay! (pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 2, :amount 1M}))))))
    (is (= ::pt/not-parsed (:type (thrown-data #(pay! (pt/parse (pt/draft Memo {:text "hi"})))))))
    (is (= [85M 20M 5M] (map #(get-in @accounts [% :balance]) [1 2 3])) "nothing moved"))
  (testing "parsed and parsed? take a sum"
    (is (pt/parsed? Payment (pt/parse (pt/draft Transfer {:user-id :ada, :from-id 1, :to-id 2, :amount 1M}))))
    (is (not (pt/parsed? Payment (pt/parse (pt/draft Memo {:text "hi"})))))))

;;; A fetch that leaves out a tuple

(defn- forgetful-rows [tuples] (into {} (map (fn [t] [t {:ok true}])) (rest tuples)))

(pt/defparsed Forgot [id]
  (external ::row-ok "The row is ok." [] [row ::row] (true? (:ok row)))
  (fetch ::row [id] forgetful-rows))

(pt/defparsed ById [id]
  (external ::by-id-found "The row exists." [] [row ::row] (some? row))
  (fetch ::row [id] (fn [tuples] (into {} (map (fn [[id]] [id {:id id}])) tuples))))

(deftest a-fetch-must-answer-every-tuple-test
  (testing "a missing tuple throws, never a nil row a check could pass on"
    (is (= ::pt/bad-fetch (:type (thrown-data #(pt/parse-all [(pt/draft Forgot {:id 1}) (pt/draft Forgot {:id 2})])))))
    (is (= (str "Fetch :parsed-types.bank-test/row returned no entry for [[1]]. A fetch answers every tuple it is given,"
                " so no check reads a nil it did not mean. Return a key for every tuple, with nil for not found, as in"
                " (into {} (map (fn [t] [t (lookup t)])) tuples).")
           (try (pt/parse-all [(pt/draft Forgot {:id 1}) (pt/draft Forgot {:id 2})]) nil
                (catch clojure.lang.ExceptionInfo e (ex-message e))))))
  (testing "a fetch keyed by the value, not the tuple, is shown the keys it returned"
    (is (re-find #"returned no entry for \[\[1\]\], and has keys such as \[1\]: each key is a tuple"
                 (try (pt/parse (pt/draft ById {:id 1})) nil
                      (catch clojure.lang.ExceptionInfo e (ex-message e)))))))
