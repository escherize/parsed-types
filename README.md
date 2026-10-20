# parsed-types

Parse, don't validate, for Clojure. A draft is a record that declares its checks. `parse` runs them and returns a
value of a separate parsed type, or throws. A function that must only ever see checked data takes the parsed type,
and `(pt/parsed SomeType x)` refuses anything `parse` did not issue.

```clojure
(require '[parsed-types.core :as pt])

(pt/defparsed TableRename [user-id table-id name])

(def ^:private table-rename-declaration
  {:parses-to TableRename
   :checks    [{:name  ::name-not-blank
                :doc   "The name is a non-blank string."
                :kind  :pure
                :reads [:name]
                :f     (fn [name] (and (string? name) (not (str/blank? name))))}
               {:name    ::may-write-table
                :doc     "The acting user may write the table."
                :kind    :external
                :reads   [:user-id]
                :fetches [::table]
                :f       (fn [user-id table] (may-write? user-id table))}]
   :fetches   [{:name  ::table
                :reads [:table-id]
                :f     (fn [arg-tuples] (tables-by-id-tuple arg-tuples))}]})

(defrecord TableRenameDraft [user-id table-id name]
  pt/Draft
  (declaration [_] table-rename-declaration))

(defn rename-table! [rename]
  (let [^TableRename rename (pt/parsed TableRename rename)]
    (write! (.-table-id rename) (.-name rename))))

(rename-table! (pt/parse (->TableRenameDraft user-id 7 "orders")))
```

## A draft and a parsed value are different types

| | Draft | Parsed |
|---|---|---|
| Defined with | `defrecord`, implementing `pt/Draft` | `pt/defparsed` |
| Changes | `assoc`, `pt/assoc-field` | none: `assoc`, `dissoc`, `with-meta` throw |
| Reads as a map | yes | yes: `get`, keywords, `=`, `seq` |
| Proof that checks passed | none | `parse` issued it |

`pt/parsed` and `pt/parsed?` check the type and that `parse` issued that exact object. `parse` records each value it
returns in a weak identity map. A value built with `->TableRename`, an equal value built by hand, or an edn round trip
is not in it.

To change a parsed value, go back to a draft:

```clojure
(-> parsed pt/draft-of (pt/assoc-field :name "x") pt/parse)
```

`draft-of` returns a draft known to pass every pure check, so `parse` reruns only the checks that read `:name`, plus
the external checks.

## Checks are pure or external

| | `:pure` | `:external` |
|---|---|---|
| Reads | only its arguments | the world: a database, the current user |
| May name `:fetches` | no | yes |
| Pass remembered for the draft | yes | no, runs on every call |
| Default `:status-code` when first to fail | 400 | 403 |

A check receives the fields under `:reads`, then the fetches under `:fetches`. It passes when it returns exactly
`true`. Anything else is the failure, reported under the check's name with its `:doc`.

Run order:

1. Pure checks, fewest fields first. A check on one field runs before a check relating it to another.
2. Fetches, once every pure check passes.
3. External checks, after their fetches.

Malformed input never reaches a fetch or an external check. A check held up by a failure is listed under that
failure's `:blocked`.

## Fetches are batched

`pt/parse-all` calls each fetch once per draft type, with every distinct tuple of the fields it reads. The fetch
returns a map from each tuple to what it fetched. A missing tuple is an error. `parse-all` returns the parsed values
in order, or throws once with every failure, by index. An exception thrown by a check propagates and stops the call.

## Drafts remember which pure checks passed

- `pt/check` runs the named checks, and what they wait on.
- `pt/passed-checks` names the pure checks a draft is known to pass.
- `pt/assoc-field` keeps the passes of checks that do not read the field it sets.
- A plain `assoc` keeps none.

Passes live in a weak identity map, never on the draft, tagged with the declaration they were set under.

Warning: a remembered pass is only true while the field still holds the value that was checked. Fields read by pure
checks must be immutable. A `java.util.HashMap`, or a `proxy` that subclasses a persistent map, can change after the
check. Check the exact class when a field could be one of those.

## Declarations are checked on first use

A declaration is refused when:

- `:parses-to` is not a parsed type, or its fields differ from the draft's;
- a name is used twice, or is not a qualified keyword;
- a check has no `:doc`, no `:f`, or a `:kind` other than `:pure` or `:external`;
- a pure check names `:fetches`, or a check names an undeclared fetch;
- a check or fetch reads a field the draft does not have;
- an `:f` closes over a local;
- there are more than 64 pure checks.

The closure check refuses `constantly`, `partial`, `comp` and `memoize`, which close over their arguments: write a
`fn`. It does not see globals, so a pure check must not read an atom.

## Known limits

The proof stops mistakes in application code. It does not stop code running in the same JVM that sets out to forge
it:

- A private var is reachable by var-quote.
- `with-redefs` can replace `parse` or `parsed`.
- Reflection can call any constructor.

## Lint

The jar ships a clj-kondo config that lints `defparsed` as `deftype`. Import it with
`clj-kondo --copy-configs --dependencies --lint "$(clojure -Spath)"`.

## Test

```
clojure -X:test
```
