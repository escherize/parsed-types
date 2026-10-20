(ns parsed-types.stale-fixture.sum
  "Fixture for the reload tests: a sum of the types of another namespace."
  (:require
   [parsed-types.core :as pt]
   [parsed-types.stale-fixture.types])
  (:import
   (parsed_types.stale_fixture.types Credit Debit)))

(pt/defsum Txn [Debit Credit])
