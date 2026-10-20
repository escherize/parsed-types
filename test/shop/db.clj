(ns shop.db
  "The store the README's examples run against, in `parsed-types.readme-test`: a fixed catalog and customer list, and
  writes that only say what they did.")

(def ^:private products {7 {:id 7, :price 5M}, 8 {:id 8, :price 3M}})

(def ^:private customers {1 {:id 1, :can-order? true}, 2 {:id 2, :can-order? false}})

(defn product [id] (get products id))

(defn customer [id] (get customers id))

(defn insert-order! [_customer-id _lines _subtotal] :placed)

(defn charge-card! [_card-token _amount] :charged)

(defn send-invoice! [_customer-id _amount] :invoiced)
