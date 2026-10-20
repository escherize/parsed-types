(ns parsed-types.stale-fixture.parent
  "Fixture for the reload tests: parents of the sum, and of a type, of other namespaces."
  (:require
   [parsed-types.core :as pt]
   [parsed-types.stale-fixture.sum :refer [Txn]]
   [parsed-types.stale-fixture.types])
  (:import
   (parsed_types.stale_fixture.types Debit)))

(pt/defparsed Entry [lines] (children lines Txn))

(pt/defparsed One [debit] (child debit Debit))
