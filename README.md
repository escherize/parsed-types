# parsed-types

A function that acts on checked data takes a parsed value, and only `parse` makes one. Holding a parsed value proves
its checks passed, for exactly the fields you read from it.

Code that validates checks a map and then passes the same map along. Nothing ties the check to the code that relies
on it, so three bugs are easy to write: check one id and use another, forget the check, or change the map after the
check. In a typed language you'd [parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/):
turn input into a type that only checked data can have, and let the compiler refuse the rest. This library gives
Clojure that type, and refuses the rest at run time.

## Two types for each thing: a draft and a parsed value

| Term | Meaning |
|---|---|
| **Draft** | Unchecked input, such as `TransferDraft`. Anyone builds one. Its fields are fixed. |
| **Parse** | Run every check on a draft. Return a new parsed value, or throw with every failure. |
| **Parsed value** | Checked output, such as `Transfer`. Only `parse` makes one, and it can't change. |
| **Check** | A function of named fields that passes by returning exactly `true`. *Pure* checks read only fields; *external* checks read the world. |
| **Fetch** | A batched lookup that external checks use, called once for many drafts. |
| **Sink** | A function that acts, such as `transfer!`. It accepts only parsed values. |

```
  ->TransferDraft, pt/draft
            |
            v
     +---------------+   pt/parse: every check passes   +-----------+
     | TransferDraft | -------------------------------> | Transfer  | --> sinks: (pt/parsed Transfer x)
     |    (draft)    | <------------------------------- | (parsed)  |
     +---------------+           pt/draft-of            +-----------+
       ^         |                                        assoc, dissoc: throw
       | assoc   | pt/parse: a check fails
       +---------+ ---> throws {:status-code 400, :failures {...}}
```

| | Draft | Parsed value |
|---|---|---|
| Make one | `->TransferDraft`, `pt/draft` | only `pt/parse` |
| Read a field | `(:amount d)`; a key that isn't a field throws | the same; `pt/let` checks keys when compiling |
| Change it | `assoc`, keeping the passes of checks that don't read the field | throws: `pt/draft-of`, `assoc`, `pt/parse` again |
| A sink accepts it | no | yes |

## Example: a transfer between accounts

```clojure
(require '[parsed-types.core :as pt])

(pt/defparsed Transfer [user-id from-id to-id amount]
  (pure ::amount-positive "The amount is a positive decimal."
        [amount]
        (and (decimal? amount) (pos? amount)))
  (pure ::two-accounts "Money moves between two different accounts."
        [from-id to-id]
        (not= from-id to-id))
  (external ::owns-source "The user owns the source account."
            [user-id] [source ::source]
            (= user-id (:owner source)))
  (fetch ::source [from-id] accounts-by-id))

(defn transfer! [transfer]
  (pt/let [{:keys [from-id to-id amount]} (pt/parsed Transfer transfer)]
    (db/move-money! from-id to-id amount)))

(defn handle-transfer [{:keys [user-id body]}]
  (transfer! (pt/parse (pt/draft Transfer {:user-id user-id
                                           :from-id (:from body)
                                           :to-id   (:to body)
                                           :amount  (:amount body)}))))
```

Each bug from above now fails:

| Bug | What happens |
|---|---|
| A caller skips the checks: `(transfer! {:from-id 1, :to-id 2, :amount 1000M})` | throws a 500: `parse` didn't make it |
| A caller builds the type by hand: `(transfer! (->Transfer :ada 1 2 1000M))` | throws a 500, for the same reason |
| A caller changes it after the check: `(assoc transfer :from-id 9)` | throws: a parsed value can't change |
| The sink reads a field that doesn't exist: `{:keys [fromid]}` | doesn't compile |
| The handler misspells a field: `{:from (:from body) ...}` | doesn't compile |

`defparsed` defines `Transfer`, its draft `TransferDraft`, and the checks between them. A check's argument vector is
the fields it reads; using any other field doesn't compile. An external check also binds fetches, `[source ::source]`,
like `let`.

`pt/parsed` returns its argument if `parse` made it, and throws otherwise. `pt/let` is `let`, plus a compile-time
check of the keys destructured from `(pt/parsed Type x)`.

## Parse runs pure checks first, and touches the world only if they all pass

1. Pure checks, a field's own checks before checks that relate two fields.
2. Fetches. A fetch takes every distinct tuple of the fields it reads and returns a map from each tuple to a result:
   `[[1] [7]] -> {[1] {...}, [7] {...}}`.
3. External checks.

Malformed input never queries the database. A failure reports every check by name:

```clojure
(pt/parse (->TransferDraft :ada 1 1 -5M))
;; Invalid Transfer
{:status-code 400
 :failures    {::amount-positive {:doc "The amount is a positive decimal.", :returned false, :blocked [::owns-source]}
               ::two-accounts    {:doc "Money moves between two different accounts.", :returned false, :blocked [::owns-source]}}}
```

`:blocked` names the checks that didn't run because of this failure. The status is 400 for a pure check and 403 for
an external one, unless the check sets `{:status 404}`.

## A draft can hold drafts, and one parse covers the whole tree

```clojure
(pt/defparsed Payroll [user-id from-id transfers]
  (children transfers Transfer)
  (pure ::no-repeat-payee "No account is paid twice."
        [transfers]
        (= (count transfers) (count (set (map :to-id transfers)))))
  (external ::covers-total "The account holds the sum of every transfer."
            [transfers] [source ::source]
            (<= (reduce + 0M (map :amount transfers)) (:balance source 0M)))
  (fetch ::source [from-id] accounts-by-id))
```

`transfers` holds `TransferDraft`s, and `parse` checks the payroll and every transfer in it together:

- **Children first.** A parent check sees each child as a checked value. When a child fails, the failure appears at
  its path, `{:transfers {3 {...}}}`, and the parent checks that read it don't run.
- **One query per fetch, not per child.** The 50 transfers' `::source` lookups are one call with 50 ids.
- **Bottom-up result.** The parsed `Payroll` holds parsed `Transfer`s, so `run-payroll!` can hand each one to
  `transfer!`.

`(child field Type)` declares a field that holds one draft, or nil.

## A sum is a closed set of parsed types

```clojure
(pt/defsum Payment [Transfer Payroll])

(defn pay! [payment]
  (pt/match Payment payment
    Transfer t (transfer! t)
    Payroll  p (run-payroll! p)))
```

A `match` that leaves out a member, or names a type that isn't one, doesn't compile. A sum works wherever a type
does: `pt/parsed`, `pt/let`, and `(children payments Payment)`.

## Edit a draft with `assoc`

A draft remembers which pure checks it passed. `assoc`, `assoc-in`, `update` and `merge` keep the passes of the checks
that don't read the changed field, so `parse` reruns only what the edit could affect, plus every external check. To
change a parsed value, go back to a draft:

```clojure
(-> transfer pt/draft-of (assoc :amount 45M) pt/parse)
```

## What it doesn't protect against

- **Code in the same JVM that sets out to forge proof:** var-quote into private state, `with-redefs`, or reflection.
  The library stops mistakes, not attackers inside the process.
- **A check that reads something it didn't name:** an atom, or a mutable value inside a field. Pure checks are
  remembered, so they must depend only on immutable fields.
- **Time between check and use:** a permission revoked after `parse` isn't seen. Parse right before you act.

## API

| Name | Does |
|---|---|
| `defparsed` | defines `Name`, `NameDraft`, and the checks |
| `draft` | builds a draft from a map; a literal map must name every field, and only fields |
| `parse`, `parse-all` | checks drafts and returns parsed values, or throws with every failure |
| `parsed`, `parsed?` | returns or tests a value `parse` made |
| `let` | `let`, checking the keys of `(pt/parsed Type x)` when compiling |
| `draft-of` | turns a parsed value back into a draft, known to pass its pure checks |
| `check`, `check-all`, `passed-checks` | runs or inspects some checks on a draft |
| `defsum`, `match` | a closed set of parsed types, and a branch on it |

[docs/guarantees.md](docs/guarantees.md) lists each guarantee with the test that proves it, every error caught at
compile time, how REPL reloads behave, and the design decisions behind them.

```
clojure -X:test
```
