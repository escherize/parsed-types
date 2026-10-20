(ns parsed-types.stale-fixture.types
  "Fixture for the reload tests in `parsed-types.defparsed-test`: the member types, which the tests reload."
  (:require
   [parsed-types.core :as pt]))

(pt/defparsed Debit [amount] (pure ::debit-pos "The amount is positive." [amount] (pos? amount)))

(pt/defparsed Credit [amount] (pure ::credit-pos "The amount is positive." [amount] (pos? amount)))
