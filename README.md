# parsed-types

Clojure code often checks a map, then passes the same map along, and nothing ties the check to the code that relies
on it. In parsed-types, `pt/parse` turns a map into a parsed type, and only `parse` can make one, so the value
carries its own proof. A function that acts on checked data, such as `place-order!`, reads it only through
`pt/parsed`, which refuses anything `parse` didn't make. Three values decide each choice: what you read in the
source is what runs, the type says what is passed around, and every error states the fix as code.

The idea is the smart constructor, or
[parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/), for Clojure.

## Install it as a git dependency

parsed-types needs Clojure 1.11 or later, and is tested on JDK 21. Add it to `deps.edn`:

```edn
{:deps {io.github.escherize/parsed-types {:git/tag "v0.3.2" :git/sha "29cfd1e"}}}
```

The API may change before 1.0. Report bugs and questions at
[GitHub issues](https://github.com/escherize/parsed-types/issues).

## Only `parse` makes a parsed value

`defparsed` defines `Quantity`, the parsed type, and its checks. `restock!` takes only a `Quantity`:

```clojure
(ns warehouse
  (:require [parsed-types.core :as pt]))

(pt/defparsed Quantity [n]
  (pure ::positive "The quantity is a positive integer."
        [n]
        (pos-int? n)))

(defn restock! [quantity]
  (let [{:keys [n]} (pt/parsed Quantity quantity)]
    [:restocked n]))

(restock! (pt/parse Quantity {:n 3})) ;; => [:restocked 3]
(pt/parse Quantity {:n 0})            ;; throws ::pt/invalid
(restock! {:n 3})                     ;; throws ::pt/not-parsed
```

`pt/parsed` checks the proof and returns the fields as a plain map. It is the only way to read a parsed value, so a
sink that skips it fails on the first real value:

```clojure
(defn restock-bad! [quantity]
  ;; reads without pt/parsed
  (let [{:keys [n]} quantity]
    [:restocked n]))

(restock-bad! (pt/parse Quantity {:n 3}))  ;; throws ::pt/must-unwrap
```

A plain map still passes `restock-bad!`, since nothing stops code from destructuring a map. Real callers pass parsed
values, though, so the first test with one fails.

A check passes only when its body returns exactly `true`. `nil`, a seq or a number fails, so wrap a truthy body in
`(boolean ...)`.

## A shop declares its checks once, and `place-order!` accepts only a parsed order

A real order also checks the world: each product exists, and each price is the catalog's. An order holds order lines
that are already parsed. `shop.db` stands in for your store ([test/shop/db.clj](test/shop/db.clj)): product 7 costs
5M and product 8 costs 3M, customer 1 may order and customer 2 may not, and writes return a keyword.

```clojure
(ns shop
  (:require [parsed-types.core :as pt]
            [shop.db :as db]))

;; your lookups: a map from each tuple to its result, nil when nothing exists.
;; Each is called once per parse-all with every tuple, so a real store runs one query here.
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
  (fetch ::product [product-id] products-by-id)
  (external ::product-exists "The product is in the catalog."
            [] [product ::product]
            (some? product))
  (external ::price-matches "The unit price is the catalog's price."
            [unit-price] [product ::product]
            (= unit-price (:price product))))

(defn line-total [line]
  (let [{:keys [quantity unit-price]} (pt/parsed OrderLine line)]
    (* quantity unit-price)))

(pt/defparsed Order [customer-id lines subtotal]
  (pure ::lines-parsed "The order has lines, and each is a parsed OrderLine."
        [lines]
        (and (vector? lines) (seq lines) (every? #(pt/parsed? OrderLine %) lines)))
  (pure ::subtotal-adds-up "The subtotal is the sum of the line totals."
        [lines subtotal]
        (= subtotal (reduce + 0M (map line-total lines))))
  ;; through the var, so a test can stub it
  (fetch ::customer [customer-id] #'customers-by-id)
  (external ::customer-can-order "The customer exists and may order."
            [] [customer ::customer]
            (true? (:can-order? customer))))

(defn place-order! [order]
  (let [{:keys [customer-id lines subtotal]} (pt/parsed Order order)]
    ;; plain maps for the store
    (db/insert-order! customer-id (mapv #(pt/parsed OrderLine %) lines) subtotal)))

(defn handle-order [{:keys [customer-id body]}]
  (let [lines (pt/parse-all OrderLine (mapv (fn [l] {:product-id (:product l)
                                                     :quantity   (:qty l)
                                                     :unit-price (:price l)})
                                            (:lines body)))]
    (place-order! (pt/parse Order {:customer-id customer-id
                                   :lines       lines
                                   :subtotal    (:subtotal body)}))))

(handle-order {:customer-id 1
               :body        {:lines [{:product 7, :qty 2, :price 5M} {:product 8, :qty 1, :price 3M}]
                             :subtotal 13M}})
;; => :placed
```

A check's argument vector names the fields it reads. An `external` check also binds fetches, as in
`[product ::product]`. `(fetch ::product [product-id] f)` calls `f` once with every distinct `[product-id]` tuple,
such as `[[7] [8]]`, and returns a map from each tuple to its row, with nil for a tuple that found nothing. A tuple
missing from the map throws `::bad-fetch`. A clause is `pure`,
`external` or `fetch`; `(doc pt/defparsed)` has the details.

## A value built any other way fails at the sink

```clojure
(def line (pt/parse OrderLine {:product-id 7, :quantity 2, :unit-price 5M}))

(line-total line)                                     ;; => 10M
;; the 4th argument is the hidden proof field: only parse fills it
(= line (OrderLine. 7 2 5M nil))                      ;; => false
(place-order! {:customer-id 1, :lines [line], :subtotal 10M}) ;; throws ::pt/not-parsed
(place-order! (Order. 1 [line] 10M nil))              ;; throws ::pt/not-parsed
(assoc line :quantity 3)                              ;; throws ::pt/cannot-change
;; read only through pt/parsed
(:quantity line)                                      ;; throws ::pt/must-unwrap
```

## Explicitness: what you read in the declaration is what runs

### Checks run in the order written, and the first failure stops the parse

A body that uses a field its argument vector doesn't name doesn't compile. Pure checks run first, then fetches,
then external checks, each in the order written. A pure check written after an external check doesn't compile, so
the source shows that order; a fetch may be written anywhere, and reads best before the checks that use it. The
first check that fails, or throws, stops the parse, and `::invalid` names it:

```clojure
(pt/parse Order {:customer-id 1, :lines [line], :subtotal 12M})
;; throws ::pt/invalid with this ex-data:
{:check    :shop/subtotal-adds-up
 :doc      "The subtotal is the sum of the line totals."
 :kind     :pure
 :returned false}
```

So a check on fewer fields goes first: a pure check written before one that reads a proper subset of its fields
doesn't compile, and the check that fails first names the root cause. An inferred dependency graph was rejected,
because it would run checks in an order the source doesn't show.

Reporting every failure was rejected. A caller learns one failure per attempt, but nothing runs after a failed
permission check, so no later check reports on data the caller may not see. Write the check that a row exists, or
that the caller may see it, before the checks that read the row:

```clojure
(pt/parse OrderLine {:product-id 99, :quantity 1, :unit-price 5M})
;; throws ::pt/invalid with this ex-data:
{:check :shop/product-exists, :kind :external}
```

### The map to parse names every field, and only fields

A missing or unknown key throws `::bad-input`, and an unknown key names the field it is close to, so a misspelled key
never parses as nil. Write
nil for a field you mean to leave empty. A literal map in `(pt/parse Order {...})` is checked when the call compiles;
any other map when it runs:

```clojure
(pt/parse OrderLine {:product-id 7, :quantity 2, :unit-price 5M, :note "gift"})
;; throws ::pt/bad-input with this ex-data:
{:rule    :parsed-types.rule/input-unknown-keys
 :unknown [:note]}
```

There is no `->OrderLine`, which is positional, so reordering a type's fields can't swap two values. A second type
for unchecked input was rejected: a plain map is unchecked input, and the parsed type is the only other one.

### Pure checks run before anything touches the world

A map that fails a pure check fetches nothing. `parse-all` runs every map's pure checks before any fetch, so one
malformed map in the batch fetches nothing for any of them:

```clojure
(pt/parse OrderLine {:product-id 7, :quantity 0, :unit-price 5M})
;; throws ::pt/invalid with this ex-data:
{:check :shop/quantity-positive, :kind :pure, :returned false}
```

That holds per call. `handle-order` parses and fetches the lines before the order's own pure checks run.

### A parent holds parsed values, and `parse-all` batches the parts' fetches

`:lines` holds `OrderLine` values that `parse-all` issued. Each fetch runs once per call, with each distinct tuple
once, so the product lookups of 50 lines are one call to `products-by-id`. A field may hold issued values, which are
not checked again. There are no child clauses: a parent holds parsed values, and parse-all batches the children's fetches. Child clauses
that parsed a whole tree in one call were rejected: they doubled the library and its edge cases to save one call.

### Nothing is cached between parses, and types match by class, not by name

`parse` runs every check every time. A cache of passed checks was rejected: it is hidden state beside the value.
Types compare by class identity, like records. Matching types by name was rejected: it let a class of the same name,
old or from another namespace, pass for the real one.

## Clarity: one name per thing, and the type says what is passed around

### The type says whether a value was checked

A plain map is unchecked input, and `Order` is checked. A validated plain map was rejected: nothing in it says
whether it was checked. There is no `->Order`, and `(Order. ...)` carries no proof: a hidden field that holds a
private token only `parse` sets. A side table of issued values was rejected, because it keeps the proof apart from
the value. Equality reads the proof, so a value built by hand never equals a parsed one.

### Fields are fixed, and values never change

A parsed value never changes. For a changed copy, parse a changed map:
`(pt/parse OrderLine (assoc (pt/parsed OrderLine line) :quantity 3))`, which runs every check again. A record was
rejected, because `assoc` on a record returns
another record of the same class, which would look like proof. `parse` refuses a field that holds, anywhere inside
it, a value not known to be immutable, such as an atom or a lazy seq: the accepted classes, JDK values such as `URI`
and `Locale` included, are [listed here](docs/guarantees.md#accepted-field-values). The walk costs about 25 microseconds
for 1,000 numbers in 100 vectors; an issued value in a field is not walked again. `clojure.walk` over a parsed value throws instead of
dropping the proof.

### A sum lists its members, and `match` handles each one

`defsum` names a closed set of parsed types. A `match` that leaves out a member doesn't compile, so adding a member
to the sum breaks every `match` that misses it. A payment is a card charge or an invoice, and nothing else:

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
    Card    card    (let [{:keys [card-token amount]} (pt/parsed Card card)]
                      (db/charge-card! card-token amount))
    Invoice invoice (let [{:keys [customer-id amount]} (pt/parsed Invoice invoice)]
                      (db/send-invoice! customer-id amount))))

(pay! (pt/parse Card {:card-token "tok_1", :amount 13M})) ;; => :charged
```

## Good for agents: a mistake fails early, and its error states the fix

### Declaration mistakes fail when the file compiles or loads

These don't compile: an unknown or missing key in a literal map passed to `pt/parse`, a `match` that misses a member, a pure check in the
wrong order, a check that uses a field it doesn't name, and a `defparsed` that isn't at the top level of a
namespace. They reach a `try` as a `CompilerException`, so read `(ex-data (ex-cause e))` for the `:type` and `:rule`.
A fetch that isn't a fn or a var holding one throws when the file loads. The library ships a clj-kondo
config, so the editor flags a check that uses a field it didn't name. Install it with:

```sh
mkdir -p .clj-kondo
clj-kondo --lint "$(clojure -Spath)" --dependencies --copy-configs --skip-lint
```

### Every error has a `:type`, a `:rule` and the fix as code

Each error is an `ex-info`. Its ex-data has a `:type` in `parsed-types.core` and a `:rule` in `parsed-types.rule`.
The message states what happened, the rule, and the fix as code:

```
get :quantity on an OrderLine: a parsed value is read only through pt/parsed, which checks the proof. Write (let
[{:keys [product-id quantity unit-price]} (pt/parsed OrderLine x)] ...).
```

In summary, with every rule in [docs/guarantees.md](docs/guarantees.md#errors):

| `:type` | Thrown when |
|---|---|
| `::invalid` | the first check that failed or threw: `:check`, `:doc`, `:kind` (`:pure` or `:external`), and `:threw` or, for a pure check, `:returned`; from `parse-all`, also the map's `:index` |
| `::not-parsed` | a sink or `match` got a value `parse` didn't make |
| `::must-unwrap` | code read a parsed value without `pt/parsed` |
| `::stale-type` | a class from before a REPL re-evaluated a `defparsed` met one from after |
| `::cannot-change` | code called `assoc`, `conj`, `with-meta` or `empty` on a parsed value |
| `::no-field` | code set a key that isn't a field |
| `::bad-input` | the map to parse has a missing or unknown key; a field holds a value not known to be immutable or an unissued value; a library fn got an argument of the wrong kind |
| `::bad-declaration` | a `defparsed`, `defsum` or `match` is malformed, found when it expands or loads |
| `::bad-fetch` | a fetch returned no map, or no entry for a tuple |

The ex-data says what kind of failure it is. `:threw` means a check has a bug. `:kind :pure` means the caller's own
input is malformed, so it's safe to show. `:kind :external` means a check of the world refused the input, so tell the
caller no more than that:

```clojure
(defn place [request]
  (try
    [:placed (handle-order request)]
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [type threw kind check doc]} (ex-data e)]
        (cond
          (not= ::pt/invalid type) (throw e)
          threw                    [:bug check]
          (= :pure kind)           [:bad-input {:check check, :doc doc}]
          :else                    [:refused])))))

(place {:customer-id 1, :body {:lines [{:product 7, :qty 0, :price 5M}], :subtotal 0M}})
;; => [:bad-input {:check :shop/quantity-positive, :doc "The quantity is a positive integer."}]
(place {:customer-id 2, :body {:lines [{:product 7, :qty 1, :price 5M}], :subtotal 5M}})
;; => [:refused]
```

Never show the caller which external check failed. If a check that reads private data is written before the
permission check, its failure reveals that data. The library keeps what an external check returned out of the
ex-data and the message, which names only its class, so a logged error holds no fetched data.

### Nothing fails silently

- **A read without `pt/parsed` throws.** `(:k x)`, `get`, destructuring, `seq` and `count` on a parsed value throw
  `::must-unwrap`, so a sink can't skip the proof check. `pt/parsed` returns a plain map, where a misspelled key reads
  nil, as in any map.
- **A missing or unknown key in the map to parse throws,** instead of parsing as nil. An unknown key names the field
  it is close to.
- **A check passes only on exactly `true`.** For a truthy result such as `(seq xs)`, the message says to wrap the
  body in `(boolean ...)`.
- **An external check gets nil when a fetch found nothing.** The first check over a fetch tests for that, as
  `::product-exists` does. When a check fails and every fetch it binds found nothing, the message says so.
- **A check body that throws stops the parse,** as a failure does. `:threw` holds the exception, which is also the
  cause. An exception from a fetch propagates.
- **There is no boolean `valid?`.** A caller can ignore a boolean, but not a throw.
- **A test stubs a fetch through its var.** `#'customers-by-id` is declared as a var, so
  `(with-redefs [customers-by-id stub] ...)` reaches it.

### Types reload like records, and `::stale-type` names what to re-evaluate

Re-evaluating a `defparsed` makes new classes, as for a `defrecord`. Where a class doesn't match but its name does,
the library throws `::stale-type`, naming what to re-evaluate or build again, such as `(require 'shop :reload)`.

## The library stops mistakes, not attackers or races

- **Code that sets out to forge can**, with reflection, a private var of `parsed-types.core`, or `with-redefs`.
- **A pure check can call the world.** The library can't stop a check body from calling a database.
- **A collection built over a Java array can share it.** `vec` of an array and Java constructors such as
  `PersistentArrayMap.` keep the array. Use `(into [] a)`, which copies.
- **Evaluate a `defparsed` on one thread at a time.** Two evaluations at once fail closed: the type is unloaded, or
  the second throws `fetches-not-staged`. Neither registers a type without its fetches.
- **External checks can go stale.** A price that matched at `parse` can change later. Parse right before you act.

## The API is eight names

| Name | Does |
|---|---|
| `defparsed` | defines `Name` and its checks |
| `parse`, `parse-all` | check maps of a type's fields and return parsed values, or throw for the first check that fails |
| `parsed` | returns the fields of a value `parse` made, as a plain map: the only way to read one |
| `parsed?` | tests whether `parse` made a value |
| `defsum`, `match` | a closed set of parsed types, and a branch over every member |
| `declaration` | returns the declaration `defparsed` wrote: its checks and fetches, each with its compiled fn |

`defparsed` defines a class, not a var. Another namespace requires the namespace that defines them, then imports them,
in that order. A sum is a var:

```clojure
(ns checkout
  (:require [parsed-types.core :as pt]
            [shop])
  (:import [shop Card Order OrderLine]))

(pt/parsed? OrderLine shop/line)                         ;; => true
(pt/parsed? Order shop/line)                             ;; => false
(pt/parsed shop/Payment (pt/parse Card {:card-token "tok_1", :amount 1M}))
```

## docs/guarantees.md pins each guarantee to a test

[docs/guarantees.md](docs/guarantees.md) lists each guarantee with its test, the accepted limits, every error rule,
and REPL reloads. Run the tests with `clojure -X:test`, and the timings with `clojure -M:bench`.

## License

MIT. See [LICENSE](LICENSE).
