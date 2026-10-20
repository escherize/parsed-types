# parsed-types

Clojure code often checks a map, then passes the same map along. Nothing ties the check to the code that relies on
it. Code can check one id and use another, skip the check, or change the map after it. How does a value prove it
was checked? In parsed-types, each thing has two types: a draft that any code can build, and a parsed type that only
`parse` can make. The parsed value carries its own proof, and a sink such as `place-order!` refuses anything else.
Three values decide each choice the library makes. Explicitness: what you read in the source is what runs. Clarity:
one name per thing, and the type says what is passed around. Good for agents: mistakes fail at compile or load time
where they can, and every error the library throws states the fix as code.

The idea is the smart constructor, or
[parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/), for Clojure.

## Only `parse` turns a draft into a parsed value

`defparsed` defines `Order`, the parsed type, and `OrderDraft`, its draft. An order holds order lines that are
already parsed.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/lifecycle-dark.svg">
  <img alt="An OrderDraft goes through pt/parse to an Order, which carries the proof and goes to place-order!. A failing check throws ::invalid. An Order built by hand is refused at the sink." src="docs/lifecycle-light.svg">
</picture>

## A shop declares its checks once, and `place-order!` accepts only a parsed order

```clojure
(ns shop
  (:require [parsed-types.core :as pt]
            [shop.db :as db]))

;; your lookups: a map from each tuple to its result, nil when nothing exists
(defn products-by-id [tuples]
  (into {} (map (fn [[id :as t]] [t (db/product id)])) tuples))

(defn customers-by-id [tuples]
  (into {} (map (fn [[id :as t]] [t (db/customer id)])) tuples))

(pt/defparsed OrderLine [product-id quantity unit-price]
  (pure ::quantity-positive "The quantity is a positive integer."
        [quantity]
        (pos-int? quantity))
  (pure ::unit-price-positive "The unit price is a positive decimal."
        [unit-price]
        (and (decimal? unit-price) (pos? unit-price)))
  (external ::product-exists "The product is in the catalog."
            [] [product ::product]
            (some? product))
  (external ::price-matches "The unit price is the catalog's price."
            [unit-price] [product ::product]
            (= unit-price (:price product)))
  (fetch ::product [product-id] products-by-id))

(defn line-total [{:keys [quantity unit-price]}]
  (* quantity unit-price))

(pt/defparsed Order [customer-id lines subtotal]
  (pure ::lines-parsed "The order has lines, and each is a parsed OrderLine."
        [lines]
        (and (vector? lines) (seq lines) (every? #(pt/parsed? OrderLine %) lines)))
  (pure ::subtotal-adds-up "The subtotal is the sum of the line totals."
        [lines subtotal]
        (= subtotal (reduce + 0M (map line-total lines))))
  (external ::customer-can-order "The customer exists and may order."
            [] [customer ::customer]
            (true? (:can-order? customer)))
  ;; through the var, so a test can stub it
  (fetch ::customer [customer-id] #'customers-by-id))

(defn place-order! [order]
  (let [{:keys [customer-id lines subtotal]} (pt/parsed Order order)]
    (db/insert-order! customer-id (mapv #(into {} %) lines) subtotal)))

(defn handle-order [{:keys [customer-id body]}]
  (let [line-drafts (mapv (fn [l] (pt/draft OrderLine {:product-id (:product l)
                                                       :quantity   (:qty l)
                                                       :unit-price (:price l)}))
                          (:lines body))]
    (place-order! (pt/parse (pt/draft Order {:customer-id customer-id
                                             :lines       (pt/parse-all line-drafts)
                                             :subtotal    (:subtotal body)})))))

(handle-order {:customer-id 1
               :body        {:lines [{:product 7, :qty 2, :price 5M} {:product 8, :qty 1, :price 3M}]
                             :subtotal 13M}})
;; => :placed
```

A check's argument vector names the fields it reads. An `external` check also binds fetches, as in
`[product ::product]`. A clause is `pure`, `external` or `fetch`; `(doc pt/defparsed)` has the details.

A payment is a card charge or an invoice, and nothing else:

```clojure
(pt/defparsed Card [card-token amount]
  (pure ::amount-positive "The amount is a positive decimal."
        [amount]
        (and (decimal? amount) (pos? amount))))

(pt/defparsed Invoice [customer-id amount]
  (pure ::amount-positive "The amount is a positive decimal."
        [amount]
        (and (decimal? amount) (pos? amount))))

(pt/defsum Payment [Card Invoice])

(defn pay! [payment]
  (pt/match Payment payment
    Card    card    (db/charge-card! (:card-token card) (:amount card))
    Invoice invoice (db/send-invoice! (:customer-id invoice) (:amount invoice))))

(pay! (pt/parse (pt/draft Card {:card-token "tok_1", :amount 13M}))) ;; => :charged
```

## A value built any other way fails at the sink

```clojure
(def line (pt/parse (pt/draft OrderLine {:product-id 7, :quantity 2, :unit-price 5M})))

(line-total line)                                     ;; => 10M
(= line (OrderLine. 7 2 5M nil))                      ;; => false
(place-order! {:customer-id 1, :lines [line], :subtotal 10M})                ;; throws ::pt/not-parsed
(place-order! (pt/draft Order {:customer-id 1, :lines [line], :subtotal 10M})) ;; throws ::pt/not-parsed
(place-order! (Order. 1 [line] 10M nil))              ;; throws ::pt/not-parsed
(assoc line :quantity 3)                              ;; throws ::pt/cannot-change
(:quantty line)                                       ;; throws ::pt/no-field
```

## Explicitness: what you read in the declaration is what runs

### Checks run in the order written, on the fields they name

A body that uses a field its argument vector doesn't name doesn't compile. Pure checks run in the order written. A
pure check that fails holds every later check that shares a field with it, and a held check holds nothing:

```clojure
(pt/parse (pt/draft Order {:customer-id 1, :lines [line], :subtotal 12M}))
;; throws ::pt/invalid with these :failures:
{:shop/subtotal-adds-up {:doc      "The subtotal is the sum of the line totals."
                         :returned false
                         :blocked  [:shop/customer-can-order]}}
```

So a check on fewer fields goes first: a pure check written before one that reads a proper subset of its fields
doesn't compile. An inferred dependency graph was rejected, because it would run checks in an order the source
doesn't show.

### A draft names every field in the source

`pt/draft` takes only a literal map that names every field, and only fields, and checks the keys at compile time.
There is no `->OrderDraft`: `pt/draft` names every field, so reordering a type's fields can't swap two values. There
is no `draft-from-map`, because a map built at run time turns a missing field into a silent nil. For values read at
run time, write `{:quantity (:qty l)}`.

### Pure checks run before anything touches the world

A draft that fails a pure check fetches nothing, and its external checks are held:

```clojure
(pt/parse (pt/draft OrderLine {:product-id 7, :quantity 0, :unit-price 5M}))
;; throws ::pt/invalid with these :failures:
{:shop/quantity-positive {:doc      "The quantity is a positive integer."
                          :returned false
                          :blocked  [:shop/product-exists :shop/price-matches]}}
```

### A parent holds parsed values, and `parse-all` batches the parts' fetches

`:lines` holds `OrderLine` values that `parse-all` issued. Each fetch runs once per call, with each distinct tuple
once, so the product lookups of 50 lines are one call to `products-by-id`. A field may hold issued values, which are
not checked again, but never a draft: `parse` throws `::bad-input` and names `(pt/parse d)` or `(pt/parse-all ds)`.
There are no child clauses: a parent holds parsed values, and parse-all batches the children's fetches. Child clauses
that parsed a whole tree in one call were rejected: they doubled the library and its edge cases to save one call.

### Nothing is remembered between parses, and no type is bound late

`parse` runs every check every time. A cache of passed checks was rejected: it is hidden state beside the value.
Types compare by class identity, like records. Matching types by name was rejected: it let a class of the same name,
old or from another namespace, pass for the real one.

## Clarity: one name per thing, and the type says what is passed around

### The type says whether a value was checked

`OrderDraft` is unchecked input, and `Order` is checked. A validated plain map was rejected: nothing in it says
whether it was checked. There is no `->Order`, and `(Order. ...)` carries no proof: a hidden field that holds a
private token only `parse` sets. A side table of issued values was rejected, because it keeps the proof apart from
the value. Equality reads the proof, so a value built by hand never equals a parsed one.

### Fields are fixed, and values never change

Reading or setting a key that isn't a field throws. A record was rejected, because `assoc` on a record returns
another record of the same class, which would look like proof. `parse` refuses a field that holds, anywhere inside
it, a value not known to be immutable, such as an atom or a lazy seq: the accepted classes are
[listed here](docs/guarantees.md#behavior-worth-knowing). The walk costs about 25 us per 1,000 numbers; an issued
value in a field is not walked again. `clojure.walk` over a parsed value throws instead of dropping the proof.

### A sum lists its members, and `match` handles each one

`defsum` names a closed set of parsed types. A `match` that leaves out a member doesn't compile, so adding a member
to the sum breaks every `match` that misses it.

## Good for agents: a mistake fails early, and its error states the fix

### Declaration mistakes fail when the file compiles or loads

These don't compile: an unknown or missing key in `pt/draft`, a `match` that misses a member, a pure check in the
wrong order, a check that uses a field it doesn't name, and a `defparsed` that isn't at the top level of a
namespace. A fetch that isn't a fn or a var holding one throws when the file loads. The library ships a clj-kondo
config, so the editor flags a check that uses a field it didn't name. Install it with:

```
clj-kondo --lint "$(clojure -Spath)" --dependencies --copy-configs --skip-lint
```

### Every error has a `:type`, a `:rule` and the fix as code

Each error is an `ex-info`. Its ex-data has a `:type` in `parsed-types.core` and a `:rule` in `parsed-types.rule`.
The message states what happened, the rule, and the fix as code:

```
OrderLine has no field :quantty. Did you mean :quantity? A key that is not a field throws, never reads nil. Read one
of its fields [:product-id :quantity :unit-price], as in (:quantity x).
```

In summary, with every rule in [docs/guarantees.md](docs/guarantees.md#errors):

| `:type` | Thrown when |
|---|---|
| `::invalid` | a check failed or threw; `:failures` has each one, and `:threw` names those that threw |
| `::not-parsed` | a sink, `match` or `draft-of` got a value `parse` didn't make |
| `::stale-type` | a class from before a REPL re-evaluated a `defparsed` met one from after |
| `::cannot-change` | code changed a parsed value, or called `dissoc`, `with-meta` or `empty` on a draft |
| `::no-field` | code read or changed a key that isn't a field |
| `::bad-input` | a field holds a mutable value, a draft or an unissued value; `pt/draft` got the wrong keys; `parse` got something that isn't a draft |
| `::bad-declaration` | a `defparsed`, `defsum` or `match` has an error, or misses an argument, found when it expands or loads |
| `::bad-fetch` | a fetch returned no map, or no entry for a tuple |

`:threw` names the checks whose body threw, so a handler can send a bug in a check to its 500 before it reads
`:failed-kinds`. From `parse-all`, `:failures` and `:threw` map the index of each draft that failed to its entries:

```clojure
(defn order-route [request]
  (try
    {:status 201, :body (handle-order request)}
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [type threw failed-kinds failures]} (ex-data e)]
        (cond
          (not= ::pt/invalid type) (throw e)
          (seq threw)              {:status 500, :body "A check threw."}
          (:pure failed-kinds)     {:status 400, :body failures}
          :else                    {:status 422, :body failures})))))

(order-route {:customer-id 2, :body {:lines [{:product 7, :qty 1, :price 5M}], :subtotal 5M}})
;; => {:status 422, :body {:shop/customer-can-order {:doc "The customer exists and may order.", :returned false}}}
```

### Nothing fails silently

A key that isn't a field throws instead of reading nil. A check passes only on exactly `true`; for a truthy result
such as `(seq xs)`, the message says to wrap the body in `(boolean ...)`. An external check gets nil when a fetch
found nothing, so guard it, as `(:price product)` does. A check body that throws is recorded under `:threw`, and its
exception is the cause; the other checks of its phase still run. An exception from a fetch propagates. There is no `check` or `check-all`, because each returned a
truthy value whether the checks passed or failed. A test stubs a fetch declared through its var, as
`#'customers-by-id` is, with `(with-redefs [customers-by-id stub] ...)`.

### Types reload like records, and `::stale-type` names what to re-evaluate

Re-evaluating a `defparsed` makes new classes, as for a `defrecord`. Where a class doesn't match but its name does,
the library throws `::stale-type` with the command that fixes it, such as `(require 'shop :reload)`.

## The library stops mistakes, not attackers or races

- **Code that sets out to forge can**, with reflection, a private var of `parsed-types.core`, or `with-redefs`.
- **A pure check can call the world.** The library can't stop a check body from calling a database.
- **`vec` of a Java array can share it.** Use `(into [] a)`, which copies.
- **External checks can go stale.** A price that matched at `parse` can change later. Parse right before you act.

## The API is ten names

| Name | Does |
|---|---|
| `defparsed` | defines `Name`, `NameDraft`, and the checks between them |
| `draft` | builds a draft from a literal map that names every field, and only fields |
| `parse`, `parse-all` | checks drafts and returns parsed values, or throws with every failure |
| `parsed`, `parsed?` | returns or tests a value `parse` made |
| `draft-of` | turns a parsed value back into a draft |
| `defsum`, `match` | a closed set of parsed types, and a branch over every member |
| `declaration` | returns the declaration `defparsed` wrote, as data |

## docs/guarantees.md pins each guarantee to a test

[docs/guarantees.md](docs/guarantees.md) lists each guarantee with its test, the accepted limits, every error rule,
and REPL reloads. Run the tests with `clojure -X:test`, and the timings with `clojure -M:bench`.
