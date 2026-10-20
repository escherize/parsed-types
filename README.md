# parsed-types

Parse, don't validate, for Clojure. Checked data gets its own type, and a function that needs checked data refuses
everything else.

```clojure
(transfer! (pt/parse (->TransferDraft :ada 1 2 30M)))  ; moves the money
(transfer! (->Transfer :ada 1 2 1000M))                 ; throws: built by hand, so no checks ran
```

## Why: a check proves something only about the value it saw

Most Clojure code validates. A function checks a map, gets back `true` or an explanation, and passes the original map
along. Nothing ties the check to the code that relies on it, so these bugs are easy to write and hard to see:

- **Check one thing, use another.** The code checks that the user owns account 1, then debits account 2, because the
  id came from a different variable.
- **Forget the check.** A new caller reaches `transfer!` without the balance check every other caller does first.
- **Change the value after the check.** Someone `assoc`s a new amount onto an approved map.

Alexis King's [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) answers
this for typed languages: turn unchecked input into a type that can only hold checked data, once, at the boundary.
Everything downstream takes the checked type, and the compiler refuses the rest.

Clojure has no type checker to refuse the rest, so this library supplies three things in its place:

1. **A separate type.** You build a `TransferDraft`. `parse` returns a `Transfer`.
2. **Proof of origin.** `parse` records every value it returns. `(pt/parsed Transfer x)` throws unless `parse`
   returned that exact object, so a `Transfer` built with its constructor is refused.
3. **No changes after the check.** A parsed value is a read-only map: `assoc`, `dissoc`, `merge` and `with-meta`
   throw.

A function that starts with `(pt/parsed Transfer x)` knows every check passed, for exactly the fields it is about to
read.

## Use it

```clojure
;; deps.edn
io.github.escherize/parsed-types {:git/sha "<sha>"}
```

### Declare a draft, its parsed type, and the checks between them

```clojure
(ns bank.core
  (:require [parsed-types.core :as pt]))

(pt/defdraft TransferDraft -> Transfer [user-id from-id to-id amount]
  ;; type: each field on its own
  (pure ::amount-positive "The amount is a positive decimal."
        [amount]
        (and (decimal? amount) (pos? amount)))
  (pure ::from-id-positive "The source account id is a positive integer."
        [from-id]
        (pos-int? from-id))
  (pure ::to-id-positive "The destination account id is a positive integer."
        [to-id]
        (pos-int? to-id))
  ;; internal: fields against each other
  (pure ::two-accounts "Money moves between two different accounts."
        [from-id to-id]
        (not= from-id to-id))
  ;; external: the world
  (external ::source-exists "The source account exists." {:status 404}
            [] [source ::source]
            (some? source))
  (external ::owns-source "The user owns the source account."
            [user-id] [source ::source]
            (= user-id (:owner source)))
  (external ::funds-available "The source account holds the amount."
            [amount] [source ::source]
            (<= amount (:balance source 0M)))
  (fetch ::source [from-id] accounts-by-id))
```

`defdraft` defines two types with the same fields: `TransferDraft`, a record you build freely, and `Transfer`, which
only `parse` returns.

Each check names the fields it reads in its argument vector, and receives exactly those. An external check then binds
fetches, `[local fetch-name]`, like `let`. Reading a field you didn't name is a compile error, and so is naming a
field the draft doesn't have:

```
defdraft: a check uses fields its argument vector does not name: [to-id]
defdraft: a check reads fields the draft does not have: [tod-id]
```

The first holds even for a field named like a `clojure.core` function, such as `name`: `defdraft` looks for every
field by name in the check's body. A local you bind in the body with a field's name counts too; rename it.

A check passes only when it returns exactly `true`. Anything else is its failure, so a check can return data that
explains itself.

A fetch takes every distinct tuple of the fields it reads and returns a map from each tuple to its result:

```clojure
(defn accounts-by-id [arg-tuples]   ; [[1] [2]] -> {[1] {...}, [2] {...}}
  (let [rows (db/accounts-where-id-in (map first arg-tuples))]
    (into {} (map (fn [[id :as t]] [t (get rows id)])) arg-tuples)))
```

### Parse at the boundary

```clojure
(defn handle-transfer [{:keys [user-id params]}]
  (transfer! (pt/parse (->TransferDraft user-id (:from params) (:to params) (:amount params)))))
```

Who is acting is a field like any other, so a check reads it the way it reads the amount.

### Make the function that acts take the parsed type and nothing else

```clojure
(defn transfer! [transfer]
  (let [^Transfer t (pt/parsed Transfer transfer)]
    (db/move-money! (.-from-id t) (.-to-id t) (.-amount t))))
```

With the type hint, `.-from-id` resolves at compile time, so a misspelled field is a reflection warning. A keyword
lookup works too, since a parsed value is a map.

## What a failure looks like

`parse` throws an `ex-info` with every failure, by name:

```clojure
(pt/parse (->TransferDraft :ada 1 1 30))
;; Invalid Transfer
{:status-code 400
 :failures    {::amount-positive {:doc      "The amount is a positive decimal."
                                  :returned false
                                  :blocked  [::source-exists ::owns-source ::funds-available]}
               ::two-accounts    {:doc      "Money moves between two different accounts."
                                  :returned false
                                  :blocked  [::source-exists ::owns-source ::funds-available]}}}
```

- `:returned` is what the check returned.
- `:blocked` lists the checks that did not run because this one failed.
- `:status-code` comes from the first failure: the check's `{:status ...}`, else 400 for a pure check and 403 for an
  external one. It is data; nothing in the library speaks HTTP.

## Pure checks run first, and nothing touches the world until they all pass

| | `pure` | `external` |
|---|---|---|
| Reads | only its arguments | the world: a database, the current user |
| Binds fetches | no | yes |
| Pass remembered for the draft | yes | no, runs on every `parse` |
| Default status when first to fail | 400 | 403 |

Order:

1. Pure checks, fewest fields first. A check of one field's shape runs before a check relating two fields.
2. Fetches, once every pure check passes.
3. External checks, after the fetches they bind.

Malformed input never reaches a fetch, so it never queries a database. It never reaches an external check either, so
it never logs a permission failure.

## A draft can hold drafts, and one parse covers the tree

A payroll pays several people from one account. Each payment is a transfer with its own checks. Some checks only the
whole payroll can make: the total against the balance, or nobody paid twice.

```clojure
(pt/defdraft PayrollDraft -> Payroll [user-id from-id transfers]
  (children transfers Transfer)
  (pure ::one-source "Every transfer draws on the payroll's account."
        [from-id transfers]
        (every? #(= from-id (:from-id %)) transfers))
  (pure ::no-repeat-payee "No account is paid twice."
        [transfers]
        (= (count transfers) (count (set (map :to-id transfers)))))
  (external ::covers-total "The account holds the sum of every transfer."
            [transfers] [source ::source]
            (<= (reduce + 0M (map :amount transfers)) (:balance source 0M)))
  (fetch ::source [from-id] accounts-by-id))

(defn run-payroll! [payroll]
  (let [^Payroll p (pt/parsed Payroll payroll)]
    (run! transfer! (.-transfers p))))   ; each child is a parsed Transfer, so transfer! takes it
```

`(children transfers Transfer)` declares that `transfers` holds a vector of drafts that parse to `Transfer`.
`(child field Type)` declares a field that holds one draft, or nil. One `parse`:

1. runs every pure check in the tree, children before parents;
2. runs every fetch once for the whole tree, so 100 transfers cost one account lookup per fetch;
3. runs every external check;
4. builds the parsed values bottom-up, so `Payroll`'s `transfers` holds parsed `Transfer`s.

A parent check that reads `transfers` sees each child as a map of the values the child's own checks saw, and waits for
the pure checks of everything under it. When a child, or anything below it, fails, the failure appears at its path,
and the parent checks that read that child are held:

```clojure
(pt/parse (->PayrollDraft :ada 1 [(->TransferDraft :ada 1 2 10M) (->TransferDraft :ada 1 3 -5M)]))
{:status-code 400
 :failures    {:transfers        {1 {::amount-positive {:doc "The amount is a positive decimal.", :returned false, ...}}}
               ::no-repeat-payee {:doc "No account is paid twice.", :blocked-by [[:transfers 1]], ...}}}
```

Child fields hold drafts, never parsed values. A transfer parsed earlier, perhaps for another user, can't be slipped
into a payroll: every check in the tree runs at the same moment, for the same request.

A tree of one type works the same way. A budget's parts are budgets:

```clojure
(pt/defdraft BudgetDraft -> Budget [name amount parts]
  (children parts Budget)
  (pure ::amount-not-negative "The amount is a decimal of at least zero."
        [amount]
        (and (decimal? amount) (not (neg? amount))))
  (pure ::parts-fit "The parts add up to at most the amount."
        [amount parts]
        (<= (reduce + 0M (map :amount parts)) amount)))
```

A parsed root proves the whole tree, because no part could have been put in without being parsed with it.

A tree may be at most 1000 levels deep and hold at most 100,000 drafts, counting a draft each time it appears. A
larger one is refused with a 400.

When anything in a tree fails, no fetch and no external check in the tree runs. `:blocked` lists only the checks of the
same draft that a failure held up.

## Batch many drafts with `parse-all`

`(pt/parse-all drafts)` parses many drafts in one call, with each fetch called once per declaration. It returns the
parsed values in order, or throws once with every failure keyed by index.

## Edit through drafts

A draft remembers which pure checks it passed, so a form edited one field at a time doesn't rerun every check.

- `pt/check` runs the named checks, and what they wait on. Children run all their pure checks. Unless every pure check
  in the tree ran and passed, nothing in the tree fetches or runs an external check.
- `pt/passed-checks` names the pure checks a draft is known to pass.
- `pt/assoc-field` sets a field and keeps the passes of checks that don't read it. A plain `assoc` keeps none.
- `pt/parse` skips pure checks already known to pass. External checks always run.

To change a parsed value, go back to a draft:

```clojure
(-> transfer
    pt/draft-of                         ; a TransferDraft known to pass every pure check
    (pt/assoc-field :amount 45M)        ; forgets only the checks that read :amount
    pt/parse)                           ; reruns those, and every external check
```

`draft-of` goes down the tree: a parsed payroll becomes a `PayrollDraft` holding `TransferDraft`s.

## Guarantees, and what each rests on

| Guarantee | Mechanism | Tested by |
|---|---|---|
| `pt/parsed` accepts only values `parse` returned | `parse` records each value in a weak identity map, compared with `identical?` | `parse-issues-a-new-equal-value-and-nothing-else-is-parsed` |
| A parsed value means its draft type's checks passed | One draft type may parse to each parsed type | `one-draft-type-per-parsed-type-test` |
| A parsed value can't change | `assoc`, `dissoc` and `with-meta` throw | `a-parsed-value-cannot-be-changed` |
| The checks see the values that are issued | Drafts must be records, each field is read once from the record's own field, and a parent's checks see its children as those values | `fields-are-read-once-from-the-record-test`, `a-draft-must-be-a-record-test` |
| `parse` reports what the declaration says | | `parse-matches-the-model`, against a model written by hand |
| Remembered passes never change an outcome | Passes are cleared per field, lost on `assoc`, and tied to the declaration that set them | `remembered-passes-never-change-the-outcome`, `remembered-passes-never-change-a-tree-outcome`, `passes-are-not-read-under-a-replaced-declaration` |
| Malformed input never reaches a fetch or an external check, anywhere in the tree | Fetches and external checks wait on every pure check in the tree | `nothing-touches-the-world-until-every-pure-check-passes`, `nothing-in-a-tree-touches-the-world-until-every-pure-check-passes` |
| `parse-all` agrees with `parse`, across declarations and on trees | Fetches grouped by declaration | `parse-all-agrees-with-parse-across-declarations`, `parse-all-agrees-with-parse-on-trees` |
| Each fetch runs once per declaration, each tuple once | Tuples deduplicated per fetch | `parse-all-fetches-once-with-each-distinct-tuple`, `a-parsed-tree-holds-parsed-nodes-and-fetches-once` |
| A tree parses exactly when every node passes | Pure phase children-first, then fetches and external checks for the whole tree | `a-tree-parses-exactly-when-every-node-passes`, `a-failed-grandchild-holds-the-grandparent-test` |
| A deep or huge tree is refused, not a crash | The tree is walked with an explicit stack, within limits on depth and size | `tree-limits-test` |
| A check reads only the fields it names | `defdraft` compiles each check as a fn of its named fields | `compile-errors-test` |

## What it doesn't protect against

The library stops mistakes in application code. It doesn't stop code in the same JVM that sets out to forge proof:

- A private var is reachable by var-quote, including the identity map.
- `with-redefs` can replace `parse` or `parsed`.
- Reflection can call any constructor.

It can't see inside a check, either:

- **A pure check that reads a global.** A check that derefs an atom has its pass remembered after the atom changes.
- **A mutable field.** A remembered pass is only true while the field holds the value that was checked. A
  `java.util.HashMap`, or a `proxy` subclassing a persistent map, can change after the check. When a field could be
  one of these, check its exact class: neither persistent map class is final, so `instance?` isn't enough.
- **Time of check and time of use.** A permission revoked between `parse` and the write isn't seen. Parse one call
  before you act.

## Design decisions

**The parsed type is a map type, not a record.** `assoc` on a record returns another record of the same class that
looks like proof. A Potemkin `def-map-type` reads like a map and refuses to change.

**Proof is identity, not a token.** A secret in a field or in metadata can be read out of one parsed value and copied
into a forged one. An identity map holds nothing a caller can copy.

**One draft type per parsed type.** If two draft types could parse to `Transfer`, a `Transfer` would mean only that
some checks passed. A second draft type is refused when it is first used.

**A check's argument vector is its reads.** One list says what the check reads, and the compiler holds the body to
it.

**A declaration is still data.** `defdraft` writes a plain map, which `(pt/declaration draft)` returns. You can write
the map by hand and implement `pt/Draft` on a `defrecord` yourself; see the docstring of `pt/Draft`.

**Only pure checks must close over nothing.** A pure check's pass is remembered, so it must depend only on its
arguments. An external check or a fetch runs on every call, so it may close over a database handle.

**A check passes only on exactly `true`.** `nil`, a string, or a map of errors is a failure, so a check can't pass by
accident.

**Children are drafts, parsed with the parent.** All the tree's checks run at one moment, all its fetches are batched,
and a child parsed elsewhere can't be reused.

## Declarations are checked when compiled, and again on first use

`defdraft` refuses at compile time:

- a check that reads a field the draft doesn't have, names one twice, or uses one it didn't name, even one named
  like a `clojure.core` function;
- a name that isn't a qualified keyword, or is used twice;
- a check with no doc string or no body;
- a fetch binding that names no declared fetch, or binds a local with a field's name;
- a pure check that binds a fetch;
- an option other than `:status`, or a `:status` that isn't an integer;
- a field named twice, a field declared as a child twice, or a child field that isn't a field.

`parse` refuses a declaration, with `:status-code 500`, when:

- `:parses-to` isn't a parsed type, its fields differ from the draft's, or another draft type already parses to it;
- two draft types share one declaration;
- a check's or fetch's `:f` isn't a fn, or a pure check's `:f` closes over a local (this refuses `constantly`,
  `partial`, `comp` and `memoize`);
- `:reads` isn't a sequence of distinct fields of the draft;
- there are more than 64 pure checks, counting one per child field.

## API

| Name | Takes | Returns |
|---|---|---|
| `defdraft` | draft name, `->`, parsed name, fields, clauses | defines both types and the declaration |
| `parse` | a draft | a new parsed value, or throws |
| `parse-all` | drafts | a vector of parsed values, or throws once with every failure by index |
| `parsed` | a parsed type and a value | the value, or throws a 500 |
| `parsed?` | a parsed type and a value | whether `parse` returned that value |
| `check` | a draft and check names (`nil` for all) | `true`, or the failures |
| `check-all` | drafts and check names | a vector of `true` or failures |
| `passed-checks` | a draft | the pure checks it is known to pass |
| `assoc-field` | a draft, a field and a value | a draft that keeps the passes of checks not reading the field |
| `draft-of` | a parsed value | a draft, and drafts of its children, known to pass every pure check |
| `defparsed`, `Draft/declaration` | | the pieces `defdraft` writes, for declarations by hand |

## Tests

```
clojure -X:test
```

- `parsed-types.bank-test`: the examples above, with a few more checks, run against an in-memory bank.
- `parsed-types.core-test`: each behavior, each declaration error, and the ways to forge or change a parsed value we know of.
- `parsed-types.defdraft-test`: each compile error, and the declaration `defdraft` writes.
- `parsed-types.properties-test`: `test.check` properties over random drafts, random edits, mixed declarations, and
  random trees, including one against a reference model written by hand.

Each property was checked against a broken copy of the library. All of these were caught, and test.check shrank each
failure to a small example:

- fetches grouped by draft type instead of by declaration;
- an external check waiting only on its fetches;
- a truthy return counted as a pass;
- a tree fetching after one of its pure checks failed;
- remembered passes read under a replaced declaration;
- `assoc-field` keeping every pass;
- tuples not deduplicated;
- `draft-of` forgetting passes;
- a failure below a direct child not holding the parent;
- fields read through `get` instead of from the record;
- a second draft type allowed to parse to the same type;
- `check` on a subset letting the tree fetch;
- no limit on depth.

## Lint

The library ships a clj-kondo config. `defdraft` expands for clj-kondo into the functions it compiles to, so a check
that uses a field it didn't name is an unresolved symbol in your editor, and one that names a field it doesn't use is
an unused binding. A field named like a `clojure.core` function, such as `name`, is caught when compiling, not in the
editor. Copy the config into your project with:

```
clj-kondo --lint "$(clojure -Spath)" --dependencies --copy-configs --skip-lint
```
