# parsed-types

Clojure code often checks a map, then passes the same map along. Nothing ties the check to the code that relies on
it. Code can check one id and use another, skip the check, or change the map after it. How does a value prove it
was checked? In parsed-types, each thing has two types: a draft that any code can build, and a parsed type that only
`parse` can make. The parsed value carries its own proof, and a sink such as `transact!` refuses anything else. Three
values decide each choice the library makes. Explicitness: what you read in the source is what runs. Clarity: one
name per thing, and the type says what is passed around. Good for agents: mistakes fail at compile or load time where
they can, and every error the library throws states the fix as code.

The idea is the smart constructor, or
[parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/), for Clojure.

## Only `parse` turns a draft into a parsed value

`defparsed` defines `Debit`, the parsed type, and `DebitDraft`, its draft.

```mermaid
flowchart LR
  code([any code]) -->|"->DebitDraft, pt/draft"| D["DebitDraft<br/>(draft)"]
  D -->|assoc| D
  D -->|"pt/parse: every check passes"| P["Debit<br/>(parsed, holds the proof)"]
  D -->|"pt/parse: a check fails"| F(["throws ::invalid, with every failure"])
  P -->|pt/draft-of| D
  P -->|"pt/parsed, match"| S([sinks, such as transact!])
  P -.->|"assoc, dissoc, empty"| T(["throws ::cannot-change"])
  H(["(Debit. ...) by hand"]) -.->|no proof| N(["sinks throw ::not-parsed"])
```

## A bank declares its checks once, and its sink accepts only parsed values

```clojure
(ns bank
  (:require [db]
            [parsed-types.core :as pt]))

;; your lookup: a map from each tuple to its result, nil when nothing exists
(defn accounts-by-id [tuples]
  (into {} (map (fn [[id :as t]] [t (db/account id)])) tuples))

;; debit: take money out of an account the user owns, if it holds enough
(pt/defparsed Debit [user-id account-id amount]
  (pure ::amount-positive "The amount is a positive decimal."
        [amount]
        (and (decimal? amount) (pos? amount)))
  (external ::owns-account "The user owns the account."
            [user-id] [account ::account]
            (= user-id (:owner account)))
  (external ::covers-amount "The account holds the amount."
            [amount] [account ::account]
            (<= amount (:balance account 0M)))
  (fetch ::account [account-id] accounts-by-id))

;; credit: put money into any open account
(pt/defparsed Credit [user-id account-id amount]
  (pure ::amount-positive "The amount is a positive decimal."
        [amount]
        (and (decimal? amount) (pos? amount)))
  (external ::account-open "The account exists and is open."
            [] [account ::account]
            (= :open (:status account)))
  (fetch ::account [account-id] accounts-by-id))

;; a transaction is a debit or a credit, and nothing else
(pt/defsum Transaction [Debit Credit])

(defn transact! [transaction]
  (pt/match Transaction transaction
    Debit  debit  (db/add-to-balance! (:account-id debit) (- (:amount debit)))
    Credit credit (db/add-to-balance! (:account-id credit) (:amount credit))))

(defn handle-debit [{:keys [user-id body]}]
  (transact! (pt/parse (pt/draft Debit {:user-id    user-id
                                        :account-id (:account body)
                                        :amount     (:amount body)}))))

;; a journal entry: transactions by one user whose debits and credits balance
(pt/defparsed JournalEntry [user-id lines]
  (children lines Transaction)
  (pure ::balanced "The debits and the credits add up to the same amount."
        [lines]
        (= (reduce + 0M (map :amount (filter #(instance? Debit %) lines)))
           (reduce + 0M (map :amount (filter #(instance? Credit %) lines)))))
  (pure ::one-user "Every line is made by the entry's user."
        [user-id lines]
        (every? #(= user-id (:user-id %)) lines)))

(defn post-entry! [entry]
  (run! transact! (:lines (pt/parsed JournalEntry entry))))
```

A check's argument vector names the fields it reads. An `external` check also binds fetches, as in
`[account ::account]`. `lines` holds `DebitDraft`s and `CreditDraft`s.

A value built any other way fails at the sink:

```clojure
(def debit (pt/parse (->DebitDraft :ada 1 5M)))

(= debit (pt/parse (->DebitDraft :ada 1 5M))) ;; => true
(= debit (Debit. :ada 1 5M nil))              ;; => false
(transact! {:account-id 1, :amount 5M})       ;; throws ::not-parsed
(transact! (->DebitDraft :ada 1 5M))          ;; throws ::not-parsed
(transact! (Debit. :ada 1 5M nil))            ;; throws ::not-parsed
(assoc debit :account-id 9)                   ;; throws ::cannot-change
(:acount-id debit)                            ;; throws ::no-field
```

## Explicitness: what you read in the declaration is what runs

### Checks run in the order written, on the fields they name

`defparsed` compiles each check as a fn of its argument vector. A body that uses a field the vector doesn't name
doesn't compile.

Pure checks run in the order written. A pure check that runs and fails holds every later pure check that shares a
field with it. A held check doesn't run, and holds nothing itself. An unbalanced entry fails `::balanced`, which
holds `::one-user`, since both read `lines`:

```clojure
(pt/parse (pt/draft JournalEntry {:user-id :ada
                                  :lines   [(->DebitDraft :ada 1 5M) (->CreditDraft :ada 2 3M)]}))
;; throws ::invalid, with these :failures:
{:bank/balanced {:doc      "The debits and the credits add up to the same amount."
                 :returned false
                 :blocked  [:bank/one-user]}}
```

`:blocked` sits on the check that failed, and lists what it held. So a check on fewer fields goes first: a pure
check written before one that reads a proper subset of its fields doesn't compile. An inferred dependency graph was
rejected, because it would run checks in an order the source doesn't show.

### A draft names every field in the source

`pt/draft` takes only a literal map that names every field, and only fields. It checks the keys at compile time.
There is no `draft-from-map`, because a map built at run time turns a missing field into a silent nil. For values
read at run time, write `{:amount (:amount body)}`.

### Pure checks run before anything touches the world

No fetch or external check runs until every pure check in the tree passes. A failed pure check also holds the external
checks:

```clojure
(pt/parse (->DebitDraft :ada 1 -5M))
;; throws ::invalid, with :failed-kinds #{:pure} and these :failures:
{:bank/amount-positive {:doc      "The amount is a positive decimal."
                        :returned false
                        :blocked  [:bank/owns-account :bank/covers-amount]}}
```

Each fetch runs once per declaration, with each distinct tuple once. A fetch may not read a child field, because
the children's external checks haven't run yet.

### Nothing is remembered between parses, and no type is bound late

`parse` runs every check every time. There is no cache of passed checks, because a cache is hidden state beside the
value, and a second, weaker kind of proof.

Types compare by class identity, like records, so `instance?` and type hints work inside checks. Matching types by
name was rejected: it let a value of an old class pass where code expected the new one, and a type from another
namespace with the same Java name could stand in for the real one. So there is no `pt/is?`.

The library sets no HTTP status codes. Each caller maps errors to its own responses, as
[the errors section](#every-error-the-library-throws-has-a-type-a-rule-and-a-fix) shows.

## Clarity: one name per thing, and the type says what is passed around

### The type says whether a value was checked

`DebitDraft` is unchecked input, and `Debit` is checked. A sink's argument is the proof that its checks passed. A
validated plain map was rejected: nothing in it says whether it was checked.

Only `parse` makes a parsed value. There is no `->Debit`. `(Debit. ...)` compiles, but its value carries no proof,
so a sink refuses it.

The proof is in the value: a hidden field holds a private token that `parse` sets, and nothing hands the token out.
A side table of issued values was rejected, because it keeps the proof apart from the value. Equality reads the
proof, so a value built by hand never equals a parsed one.

Only `defparsed` makes types. It defines the draft type and the parsed type in one form, so a draft type can never
claim another parsed type. There are no hand-written declarations.

### Fields are fixed, and values never change

Reading or setting a key that isn't a field throws. A record was rejected, because `assoc` on a record returns another
record of the same class, which would look like proof. There is no `pt/let`: plain `let` destructures a parsed value,
and a misspelled key already throws.

`parse` refuses a field that holds, anywhere inside it, an atom, an array, a `java.util.Date`, a clock, a mutable Java
collection, or a lazy seq. So what a check proved stays true. Realize a seq on the way in, with `vec` or `mapv`. There
is no opt-out yet.

### A walk over a parsed value fails loudly

`empty` throws, so `clojure.walk` over a parsed value throws. It never returns a plain map without the proof. Use
`(into {} x)` for a plain map.

### One parse checks a whole tree

A parent check sees each child after the child's pure checks pass. A parent check that reads a failed child is held.
Its entry has `:blocked-by [[:lines 0]]`, the paths of the children that failed. The fetches of all the children
batch: the `::account` lookups of 50 debits are one call. Parsing children apart was rejected, because a child parsed
elsewhere, perhaps for another user, could then be reused.

### A sum lists its members, and `match` handles each one

`defsum` names a closed set of parsed types. A `match` that leaves out a member doesn't compile, so adding a member
to the sum breaks every `match` that misses it.

## Good for agents: a mistake fails early, and its error states the fix

### Declaration mistakes fail when the file compiles or loads

These don't compile:

- an unknown or missing key in `pt/draft`;
- a `match` that misses a member;
- a pure check in the wrong order;
- a check that uses a field it doesn't name;
- a fetch over a child field;
- a `defparsed` that isn't at the top level of a namespace.

A child type that isn't a parsed type, or a fetch that isn't a fn, throws when the file loads.

### Every error the library throws has a `:type`, a `:rule` and a fix

Each error is an `ex-info`. Its ex-data has a `:type` in `parsed-types.core` and a `:rule` in `parsed-types.rule`.
The message states what happened, the rule, and the fix as code. A misspelled key suggests the nearest field:

```
Debit has no field :acount-id. Did you mean :account-id? A key that is not a field throws, never reads nil. Read one
of its fields [:user-id :account-id :amount], as in (:account-id x).
```

| `:type` | Thrown when |
|---|---|
| `::invalid` | a check failed; `:failures` has each one |
| `::not-parsed` | a sink, `match` or `draft-of` got a value `parse` didn't make |
| `::stale-type` | a class from before a REPL re-evaluated a `defparsed` met one from after |
| `::cannot-change` | code called `assoc`, `dissoc` or `empty` on a parsed value, or `dissoc` or `empty` on a draft |
| `::no-field` | code read or changed a key that isn't a field |
| `::bad-input` | `pt/draft` got the wrong keys, a field holds a mutable value, or a tree is too big |
| `::bad-declaration` | a `defparsed`, `defsum` or `match` has an error, found when it expands or loads |
| `::bad-fetch` | a fetch returned no entry for a tuple |

A handler maps `:type` to its own responses:

```clojure
(defn debit-route [request]
  (try
    (handle-debit request)
    {:status 200}
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [type failed-kinds failures]} (ex-data e)]
        (cond
          (not= ::pt/invalid type) (throw e)
          (:pure failed-kinds)     {:status 400, :body failures}
          :else                    {:status 403, :body failures})))))
```

### Nothing fails silently

A key that isn't a field throws instead of reading nil. A check passes only on exactly `true`, so nil, a string or a
map of errors fails. There is no `check` or `check-all`, because each returned a truthy value
whether the checks passed or failed.

### Types reload like records, and `::stale-type` names what to re-evaluate

Re-evaluating a `defparsed` at the REPL makes new classes, as for a `defrecord`. Code evaluated before keeps the old
ones. Where a class doesn't match but its name does, the library throws `::stale-type`. The message
names what to re-evaluate: the `defparsed`, the `defsum`, the namespace a `match` was compiled in, or the code that
built the old value. Where a reload fixes it, the message gives the command, such as `(require 'bank :reload)`.

## The library stops mistakes, not attackers or races

- **Code that sets out to forge can.** Reflection, any private var of `parsed-types.core`, `with-redefs` or `in-ns`
  can forge a parsed value or change the checks.
- **Reader syntax fails closed.** `#bank.Debit[...]` builds a value without the proof, and `pt/parsed` refuses it.
- **A pure check can call the world.** The library can't stop a check body from calling a database.
- **`vec` of a Java array can share it.** `(vec a)` of a Java array of 32 or fewer elements wraps the array itself.
  `parse` can't tell it from another vector. Use `(into [] a)`.
- **External checks can go stale.** A balance that passed at `parse` can change later. Parse right before you act.

## The API is ten names

| Name | Does |
|---|---|
| `defparsed` | defines `Name`, `NameDraft` with `->NameDraft`, and the checks between them |
| `draft` | builds a draft from a literal map that names every field, and only fields |
| `parse`, `parse-all` | checks drafts and returns parsed values, or throws with every failure |
| `parsed`, `parsed?` | returns or tests a value `parse` made |
| `draft-of` | turns a parsed value back into a draft |
| `defsum`, `match` | a closed set of parsed types, and a branch over every member |
| `declaration` | returns the declaration `defparsed` wrote, as data |

## docs/guarantees.md pins each guarantee to a test

[docs/guarantees.md](docs/guarantees.md) lists each guarantee with its test, every error rule, and REPL reloads.
Run the tests with:

```
clojure -X:test
```
