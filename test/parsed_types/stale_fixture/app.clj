(ns parsed-types.stale-fixture.app
  "Fixture for the reload tests: a match on the sum of another namespace."
  (:require
   [parsed-types.core :as pt]
   [parsed-types.stale-fixture.sum :refer [Txn]]
   [parsed-types.stale-fixture.types])
  (:import
   (parsed_types.stale_fixture.types Credit Debit)))

(defn transact [x] (pt/match Txn x Debit d [:debit (:amount d)] Credit c [:credit (:amount c)]))
