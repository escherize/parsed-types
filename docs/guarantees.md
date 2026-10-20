# Guarantees, limits and design

Every guarantee below is pinned by a named test, and every test was checked against a broken copy of the library.
The README covers how to use the library; this page covers what it promises and why.

## Guarantees

| Guarantee | Mechanism | Tested by |
|---|---|---|
| `pt/parsed` accepts only values `parse` made | A parsed value has a hidden `parse-proof` field. `parse` sets it to a private token. `pt/parsed` first checks that the value's class is one `defparsed` registered, by identity, then asks the value `(issuedWith token)`, which compares inside the type | `parse-issues-a-new-equal-value-and-nothing-else-is-parsed`, `parsed-refuses-anything-parse-did-not-issue-test` |
| The token never leaves the value | The field is `volatile-mutable`, which Clojure compiles to a non-public field that interop can't read, and nothing sets it after the constructor. No public method returns it. A hand-written class is refused before it is asked, so it never sees the token | `the-proof-never-leaves-the-value-test` |
| A hand-written type is refused, even with the same name | `pt/parsed` requires the class `defparsed` registered, by identity | `a-hand-written-class-with-the-same-name-is-refused-test` |
| A `defparsed` can't redefine another's classes | Each class name is claimed with its role, parsed or draft, by the namespace, as written, whose `defparsed` first defined it. A `defparsed` whose classes would take a name claimed by another namespace (`my_app.bank` for `my-app.bank`), or a name in the other role (a type `FooDraft` beside `Foo`'s draft), throws `::bad-declaration` when it expands and again when it loads, before its `deftype`s, so it defines no class | `a-class-name-belongs-to-one-namespace-test`, `a-class-name-has-one-role-test` |
| A parsed value can't be built by accident | `defparsed` removes the parsed type's `->Name` factory, so `(->Debit ...)` doesn't compile. `(Debit. a b c nil)` compiles, and `pt/parsed` refuses it | `the-positional-factory-is-removed-test`, `parsed-refuses-anything-parse-did-not-issue-test` |
| The proof is not part of the map | `parse-proof` isn't a key, and isn't in `seq`, `count`, equality, hash or the printed form. A field may not be named `parse-proof` | `the-proof-is-not-a-key-test`, `field-name-limits-test` |
| A parsed value means its draft type's checks passed | Only `defparsed` defines types. It defines the draft type and the parsed type together, so one draft type parses to each parsed type, and a class name belongs to the namespace that first defined it. `defparsed-type` and `defdraft-type` are private, so calling them doesn't compile, and a hand-made type that implements `DraftValue` has no plan | `only-defparsed-makes-types-test`, `a-draft-must-be-a-draft-type-test`, `a-hand-made-draft-type-is-refused-test` |
| A declaration error is found when the `defparsed` loads, at the latest | The macro refuses what it can see when it expands. The expansion builds and checks the plan when it loads, and throws `::bad-declaration` then | `compile-errors-test`, `declaration-errors-at-load-test` |
| A parsed value never equals a plain map, its draft, or a value built without the proof | The type's equality checks the class and the proof; it isn't marked map-equivalent, so Clojure's maps refuse it too. So `(contains? #{real} forged)` is false | `parse-issues-a-new-equal-value-and-nothing-else-is-parsed`, `parse-test`, `the-proof-is-not-a-key-test` |
| A parsed value holds only immutable values | `parse` walks every field of every draft in the tree, deeply, and accepts only values known to be immutable. Anything else throws `::bad-input` with the field, the path inside it, the class and the fix | `mutable-field-values-are-refused-test`, `a-mutable-value-in-a-child-is-refused-test` |
| A parsed value can't change | `assoc`, `dissoc`, `with-meta` and every `java.util.Map` mutator throw `::cannot-change` | `a-parsed-value-cannot-be-changed-test`, `a-parsed-value-cannot-be-changed`, `java-map-interface-test` |
| A walk over a parsed value throws, and never drops the proof | `empty` on a draft or a parsed value throws `::cannot-change`, so `clojure.walk` can't rebuild it as a plain map | `empty-throws-test`, `java-map-interface-test` |
| A misspelled key fails, never reads `nil` | Drafts, parsed values and the children a parent check sees have fixed fields; a literal `pt/draft` checks keys when compiling | `unknown-keys-test`, `a-parent-check-cannot-read-a-key-a-child-lacks-test`, `draft-test` |
| A field may be named like any `clojure.core` function | The map type's methods call every function qualified, so no field shadows one | `fields-named-like-core-functions-test`, `a-field-cannot-replace-the-checks-test` |
| Equality is a record's: same type, each field `=`, and for a parsed value the same proof | `equiv` checks the class and the proof, then compares fields with `=`, so `1.0M` equals `1.00M`. The hash reads only the fields | `equality-test`, `the-proof-is-not-a-key-test` |
| The checks see the values that are issued | Each field is read once, from the type's own Java field, by a fn `defparsed` generates. The issued value holds the same field values the checks saw | `parse-matches-the-tree-model` |
| `parse` reports exactly what the declaration says | | `parse-matches-the-model`, `parse-matches-the-tree-model`: each against a model written by hand, down to every path, `:blocked-by`, `:blocked` and `:failed-kinds` |
| Pure checks run in the order written, and a failure holds the later checks on its fields | After the shape checks of the child fields, each pure check runs in its written place, held when a pure check before it that shares a field failed. `defparsed` refuses a pure check written before one on a proper subset of its fields | `pure-checks-run-in-the-order-written-test`, `parse-matches-the-model`, `parse-matches-the-tree-model` |
| Malformed input never reaches a fetch or an external check, anywhere in a tree | Fetches and external checks wait on every pure check in the tree | `nothing-touches-the-world-until-every-pure-check-passes`, `nothing-in-a-failing-tree-fetches` |
| Each fetch runs once per declaration, with each tuple once, and answers every tuple | Tuples gathered across the call and deduplicated; a missing answer throws `::bad-fetch` | `parse-all-fetches-once-with-each-distinct-tuple`, `fetches-see-checked-values-and-run-once`, `payroll-test`, `a-fetch-must-answer-every-tuple-test` |
| `parse-all` agrees with `parse` | Fetches grouped by declaration | `parse-all-agrees-with-parse-across-declarations`, `parse-all-agrees-with-parse-on-trees` |
| Inside a check, no child is parsed | While a tree is checked, a parent's checks see each child as a value built without the token, so `pt/parsed` and `match` refuse it | `a-check-sees-children-without-the-proof-test` |
| A parsed tree is parsed all the way down, and `draft-of` returns drafts all the way down | When the whole tree passes, `parse` rebuilds it bottom-up with the token, so the issued root holds issued children | `a-check-sees-children-without-the-proof-test`, `parse-matches-the-tree-model`, `draft-of-returns-drafts-all-the-way-down` |
| Edits to child fields parse like fresh trees | `assoc` on a draft builds a new draft | `edits-to-child-fields-match-the-tree-model` |
| `match` handles every member of a sum, and only parsed values reach a branch | `match` compares its clauses with the sum when it expands, and runs a branch only when `pt/parsed?` accepts the value | `match-compile-errors-test`, `match-runs-only-on-parsed-members` |
| A parent check never runs over children that failed, in either phase | Pure and external checks that read a child field are held, with `:blocked-by`, when anything under that field failed in the same phase | `a-failed-grandchild-holds-the-grandparent-test`, `a-child-external-failure-holds-the-parent-test`, `parse-matches-the-tree-model` |
| A deep or huge tree is refused with `::bad-input`, not a crash | The tree is walked with an explicit stack; at most 1000 levels counting the root, and 100,000 drafts counting repeats | `tree-limits-test`, `depth-boundary-test`, `node-limit-boundary-test` |
| A check reads only the fields it names | `defparsed` compiles each check as a fn of its named fields, and scans its body for the others | `compile-errors-test` |
| Every error names what happened, the rule it broke and the fix | Each `ex-info` carries a `:rule` in `parsed-types.rule` next to `:type`, and its message holds the values, the rule and the fix as code. A misspelled key suggests the nearest field | `every-error-names-what-happened-the-rule-and-the-fix-test`, `every-rule-has-an-example-test`, `every-throw-names-a-rule-test`, `suggestions-are-data-too-test` |
| Only types `defparsed` defined count as types | `pt/parsed`, `pt/parsed?`, `defsum` and child fields accept a class only when `defparsed` registered it, by identity. A class that only implements `ParsedValue` is refused with `::bad-input` | `the-type-argument-is-checked-test`, `only-sum-builds-a-sum-test` |
| A sum is built only from distinct parsed types | A sum isn't a map, so `assoc` can't change one. Every use checks its members, so one built with its constructor is checked too. Sums are equal when their names and members are | `only-sum-builds-a-sum-test` |
| Reloading the library keeps every type and value defined before | The token is a `defonce`, and `ParsedValue`, `DraftValue` and `Sum` are defined only when they are not loaded yet | `reloading-the-library-keeps-every-type-test` |
| Types compare by class, like records, and a stale class says what to re-evaluate | `pt/parsed`, `parsed?`, sums, a child field's shape check and `match` compare classes by identity. Where the class differs but the name matches, they throw `::stale-type`, naming the parent `defparsed` and its namespace, the `defsum`, the namespace a `match` was compiled in, or the code that built the old value | `re-evaluating-a-child-type-test`, `re-evaluating-a-member-of-a-sum-test`, `every-error-names-what-happened-the-rule-and-the-fix-test` |

## Accepted limits

The proof makes a parsed value impossible to build by accident, and hard to forge by mistake. These ways around it,
and these gaps, are accepted:

- **Reflection can forge.** `setAccessible` reads or writes any field, the proof field included.
- **The token's var.** Code can name the private token as `@#'parsed-types.core/parse-token` and pass it to the
  constructor. The same goes for the private `register!`, which records a plan. The clj-kondo config this library
  ships flags both outside `parsed-types.core`.
- **`with-redefs` on library fns.** Rebinding `pt/parsed?` or a private fn changes what the library checks.
- **Reader syntax.** `#ns.Debit[...]` calls the constructor, so it builds a value without the proof. That fails
  closed: `pt/parsed` refuses it.
- **Mutable values are refused by a walk of known classes.** It trusts a class's interfaces: a type that implements
  `IPersistentVector` mutably passes. There is no opt-out for mutable values yet.
- **A sum can be built with its constructor.** A sum is a `deftype`, not a record, so `assoc` and `merge` can't
  make one from another, and `->Sum` is removed. `(parsed_types.core.Sum. 'S members)` still builds one, so every
  use checks the members again: `pt/parsed` and `pt/parsed?` refuse a sum whose members are not distinct
  parsed types. A sum built that way with real members is the same as one `defsum` builds.
- **`in-ns` can redefine a namespace's types.** Code that switches into a namespace with `in-ns` and runs a
  `defparsed` there redefines its types, with whatever checks it writes, the same way a REPL reload does.
- **External checks can go stale.** An external check reads the world at the moment of `parse`. A balance that
  passed can change later, so parse right before you act.

Reading the token from a real value is no longer possible without reflection.

## Errors

Every error the library throws is an `ex-info` whose ex-data has a `:type`, a keyword in `parsed-types.core`, and a
`:rule`, a keyword in `parsed-types.rule` that names the rule it broke, such as `:parsed-types.rule/got-draft`. Its
message says, in this order, what happened with the concrete values (the type, the field or key, the path, the class
it got), the rule in one clause, and the fix, as code where it can:

```
Expected a parsed Debit, got a DebitDraft, which is not checked yet. A sink takes only a value pt/parse issued. Call
pt/parse on it first, as in (pt/parse draft), and pass the result.
```

The ex-data holds the same facts as data, so no information is in the prose alone. When a key is not a field, the
message suggests the nearest field within a couple of edits, as in `Did you mean :account-id?`, and `:suggestion`
holds it. `errors-test` has one example of every rule, and fails when `core.clj` has a rule with no example, an `ex-info`
without a `:rule`, or a `throw` of anything but an `ex-info`.

| `:type` | Thrown when | Other ex-data |
|---|---|---|
| `::invalid` | `parse` or `parse-all` ran the checks and some failed. The message names the first five failed checks with their docs | `:failures`, `:failed-kinds`, and `:draft` from `parse` |
| `::not-parsed` | `pt/parsed`, `match` or `draft-of` got a value `parse` didn't issue. The rule says which: nil, a plain map, a draft of the type, an unchecked value of the type, or a value of another type | `:expected`, `:got` |
| `::stale-type` | a class doesn't match but its name does: one side is from before a REPL re-evaluated a `defparsed`. The rule says which, and the message what to re-evaluate (see "REPL reloads") | `:class`, and `:ns`, `:field` or `:parent` where they apply |
| `::cannot-change` | `assoc`, `dissoc`, `with-meta` and the like on a parsed value, `dissoc` or `with-meta` on a draft, `empty` on either, or a `java.util.Map` mutator (`put`, `putAll`, `remove`, `clear`) on either. `ex-info` is a `RuntimeException`, as `UnsupportedOperationException` is, so Java callers still get an unchecked throw, but not that class | `:parsed` or `:draft`, `:op`, `:key` |
| `::no-field` | reading a key that isn't a field, or setting one on a draft | `:parsed` or `:draft`, `:key`, `:fields`, `:suggestion` when a field is close |
| `::bad-input` | `pt/parse-all` got a single draft, a map or something that is not sequential, `pt/parsed` or `pt/parsed?` got a type argument that is not a parsed type or a sum, `pt/draft-from-map` got a class that is not a draft type, `conj` got a vector that is not a `[key value]` pair, `pt/draft` got a non-map or keys that aren't fields, `parse` got something that isn't a draft of a type `defparsed` defined, a field holds a value not known to be immutable, or a tree is over the limits | depends on the cause; `:draft`, `:field`, `:path`, `:got` for a mutable value |
| `::bad-declaration` | a declaration error, when `defparsed` expands or loads | `:draft` when found at load |
| `::bad-fetch` | a fetch returned no entry for some tuples | `:fetch`, `:missing` |

Class names in ex-data are strings, so ex-data can be serialized. For `::invalid`:

- `:failures` is shaped like the tree: the root's checks at the top, and each child's under its field (and index,
  for `children`). From `parse-all`, it maps the index of each draft that failed to its failures.
- `:failed-kinds` is a set of `:pure` and `:external`: the kinds of check that returned a failure anywhere in the
  call. A pure failure means the input is malformed. An external one means a check of the world refused it. A caller
  can map each kind to its own error codes.
- `:draft` is the name of the draft's class.

## Errors caught when compiling

`defparsed` refuses:

- a check that reads a field the draft doesn't have, names one twice, or uses one it didn't name;
- a check or fetch name that isn't a qualified keyword, is used twice, or is in `parsed-types.child`, which is
  reserved for the checks on child fields;
- a check with no doc string or no body;
- a fetch binding that names no declared fetch, binds a local with a field's name, or binds two fetches to one local;
- a pure check that binds a fetch;
- an options map: checks take no options;
- a field named twice, named `/`, like a special form, with a dot in it, starting with `__`, named `parse-proof`, or compiling
  to the same Java name as another field (`a-b` and `a_b`);
- a field declared as a child twice, or a child field that isn't a field;
- a pure check written before a pure check that reads a proper subset of its fields. Pure checks run in the order
  written, and a failed one holds every later one that shares a field, so the smaller check goes first.

`pt/draft` refuses a literal map that names a key that isn't a field, or leaves a field out. `match` refuses a missing
member, a type that isn't a member, and a member handled twice. A call to `pt/defparsed-type`, `pt/defdraft-type` or
a parsed type's `->Name` doesn't compile.

## Errors caught when a `defparsed` loads

The expansion builds and checks the plan when it loads, before any `parse`. It throws `::bad-declaration` when:

- a child field names something that isn't a type defined with `defparsed` or a sum defined with `defsum`;
- a fetch's function isn't a fn.

## Behavior worth knowing

- **Reading unknown keys.** A one-argument lookup throws: `(:k x)`, `(get x :k)`, `(x :k)` and destructuring. With a
  default, `(get x :k ::none)` returns it, and so does `:or` in `let`, which reads with a default. `contains?` is
  false, `find` is nil, `select-keys` leaves the key out, and `java.util.Map.get` returns null.
- **Equality.** Like a record, a value equals only a value of its own type with the same fields, and a parsed value
  also only one with the same proof. So equality no longer confuses a parsed value with one built by hand, but sinks
  still check with `pt/parsed`. Two values built without the proof, such as the views parent checks see, are equal
  when their fields are. Java `.equals` is one-sided, unlike a record, whose `.equals` compares contents both ways: a `HashMap` or a Clojure
  map's `.equals` compares contents and says true, while the value's own `.equals` says false. So a Java `HashSet`
  holding a parsed value can report that it contains an equal plain map.
- **No empty version.** `empty` throws, and so does anything built on `(into (empty x) ...)`, such as
  `clojure.walk`. Use `(into {} x)` for a plain map.
- **What a parent check sees.** Each child is a value of its parsed type that passed its pure checks, built without
  the token. Inside the check, `pt/parsed` refuses it; ask its type with `instance?`, as in
  `(filter #(instance? Debit %) lines)`. When the whole tree passes, `parse` builds each node again with
  the token. The issued value has the same fields as the value the checks saw, but that value lacks the proof, so the
two are not equal. A parent's pure checks run
  before any external check. A parent's external checks run after the children's, and one that reads a child that
  failed an external check is held, so it can't throw over the child's failure.
- **Every check runs every time.** `parse` and `parse-all` run every check of every draft in the tree. There is no
  way to run some of them.
- **Immutable values only.** Every field of every draft in the tree is walked, keys included, except child fields,
  whose drafts are walked as their own nodes. Accepted: nil; `Boolean`, `Character`, `String`, keywords, symbols,
  `UUID`, `Pattern`; `Long`, `Integer`, `Short`, `Byte`, `Double`, `Float`, `BigInt`, `Ratio`, and `BigDecimal` and
  `BigInteger` by exact class, since they can be subclassed; classes; final classes of the `java.time` package; values
  `parse` issued; and persistent vectors, maps (records and drafts included), sets, lists and queues whose contents
  pass. Metadata on a value is walked like the value. A sorted map or set must sort with the default comparator: a
  custom comparator can read state that changes. Refused: everything else, including lazy and other seqs, since
  realizing one could run side effects or never end; pass `(vec ...)`. For an array the message names
  `(into [] a)`, since `(vec a)` of an object array of 32 or fewer elements shares the array. Nesting deeper than 1000
  levels is refused too, and the message shows the first 8 steps of a longer path. Each collection is walked once per
  parse, by identity, so shared structure costs its size, not the number of paths to it.
- **Fetch calls.** One call per declaration and fetch for the whole call, with each distinct tuple once.
- **No-op changes.** `(merge p {})` and `(conj p nil)` return `p` itself, which stays parsed.
- **`pt/draft` with a map built at run time.** Missing fields are nil, and a key that isn't a field throws
  `::bad-input`.
- **Failed kinds in a tree.** One tree never holds both: its external checks run only when every pure check in it
  passed. `parse-all` can report both, from different drafts.
- **Exceptions.** An exception thrown by a check or fetch propagates out of `parse` and stops `parse-all` at that
  draft.

## REPL reloads

Re-evaluating a `defparsed` makes new classes with the old names. Parsed types compare by class, like records, so
code evaluated before keeps the old classes, and drafts and values built before keep theirs. A draft of the old
class still parses, with the plan it was defined with.

Where a class doesn't match but its name does, the library throws `::stale-type`. The rule says which side is old,
and the message says what to re-evaluate:

| Rule | Where | Fix the message names |
|---|---|---|
| `stale-value` | a sink, `match` or a child field got a value or draft built before its type was re-evaluated | build it again from a new draft, or re-run the code that built it |
| `stale-sink` | `(pt/parsed Debit x)` compiled before `Debit` was re-evaluated got a new value | re-evaluate the namespace that calls it, or `(clojure.tools.namespace.repl/refresh)` |
| `stale-child-type` | a parent evaluated before its child type, or its sum, was re-evaluated got a new draft | re-evaluate the `defsum`, then the parent `defparsed`, or `(require 'ns :reload)` |
| `stale-match` | a `match` compiled before a member was re-evaluated got a new value | re-evaluate the namespace the match was compiled in, as `(require 'ns :reload)` |
| `stale-sum` | a `match` names a member re-evaluated after its `defsum` | re-evaluate the `defsum`, then the match |

Two registered classes share a name only when the namespace that defined them defines them again: a REPL reload, or
code that switches into that namespace with `in-ns`. A `defparsed` in another namespace whose name compiles to the
same Java name is refused.

Reloading `parsed-types.core` itself, with `:reload` or with the `:reload-all` of a namespace that requires it, keeps
everything defined before working. The token is a `defonce`, and the library defines its interfaces and its sum class
only when they are not loaded yet. So values parsed before stay parsed, drafts built before still parse, and sums and
`match`es defined before still take their members.

## Design decisions

**Drafts and parsed values are map types with fixed fields, not records.** `assoc` on a record returns another record
of the same class, which for a parsed value would look like proof, and `map->` turns a typo into an extra key.

**Proof is in the value.** A value carries the fact that it was checked, in a hidden field that only `parse` sets to
a private token. A weak identity map of issued values did the same job, but kept that fact apart from the value. The
field isn't a key, so it changes no lookup or printing. Equality does read it, so a forgery never equals a parsed
value. The token goes into the value and only a boolean comes out. It makes a parsed value impossible to build by
accident, not impossible to forge: see "Accepted limits".

**Only immutable values.** A check proves the value it saw. If a field held an atom or an array, the parsed value
would keep something that can change under the proof. An allowlist of known immutable classes, walked all the way
down, is simpler to trust than a denylist, which misses what it doesn't name.

**No partial checks.** `check` and `check-all` ran checks without issuing anything, and a subset of names left a tree
half-proven, which needed its own rules. A failure tree is also truthy, so `(when (pt/check d nil) ...)` read as a
pass. Only `parse` and `parse-all` remain.

**Only `defparsed` makes types.** The draft type and the parsed type are defined in one form, so the draft type is the
only one that parses to its parsed type, with no table of claims. Declarations are no longer written by hand as data.
`(pt/declaration draft)` still returns the declaration `defparsed` wrote, as data.

**Plans are built when `defparsed` loads.** A declaration error shows up when the file loads, not at the first parse,
and no lazy cache is needed. A registry maps each draft class and each parsed class to its plan.

**Types compare by class, like records.** `instance?` and type hints then work, inside checks too. A reload strands
code compiled against the old class, as it does for a record, so the library detects a class whose name matches and
throws `::stale-type` with what to re-evaluate. Comparing by name kept old code working, but let a value of one
generation pass where code expected another, and needed a second way to ask a type, `pt/is?`, inside checks.

**No remembered passes.** `parse` runs every check every time. A pass remembered on a draft was a second, weaker kind
of proof, kept beside the value, and it cost bitmasks, a 64-check limit and a weak map.

**A check's argument vector is its reads.** One list says what the check reads, and the compiler holds the body to
it. A check may close over a local, and the library doesn't look: a guard against that caught only a
`defparsed` re-evaluated inside a `let`, and cost a rule and a reflective scan.

**Building and reading need no reflection.** `defparsed` knows each class and its fields when it expands, so it
generates the fns that build a parsed value, build a draft and read a draft's fields.

**Checks happen in ordinary forms.** `pt/draft` builds what `->DebitDraft` builds. It adds a compile-time check and
expands to the plain form.

**A check passes only on exactly `true`.** `nil`, a string or a map of errors is a failure, so a check can't pass by
accident.

**Children are drafts, parsed with the parent.** All the tree's checks run at one moment, all its fetches are batched,
and a child parsed elsewhere, perhaps for another user, can't be reused.

**`match` bindings carry no type hint.** After a reload, a hint would name the old class, and interop on the binding
would throw a `ClassCastException`. Read fields by keyword.

## Tests

`clojure -X:test` runs:

- `bank-test`: transfers, payrolls, budgets and payments, against an in-memory bank;
- `core-test`: each behavior, the proof, mutable values, and the ways to forge or change a value we know of;
- `defparsed-test`: compile errors, load errors, unknown keys, field names, REPL reloads;
- `properties-test`: `test.check` properties over random drafts and trees, against a model written by hand;
- `errors-test`: one example of every error rule, with the values and the fix its message must hold;
- `tree-properties-test`: `test.check` properties over random trees with `:one` and `:many` children, malformed
  children and edits to child fields, against a model of the whole failure map.

Each of these deliberately broken copies of this version fails at least one test: the proof field public; a method
handing the token out; the registry not consulted before asking a value for its proof; equality ignoring the proof;
the immutability walk shallow, skipping map keys, or accepting seqs; no did-you-mean, or one that suggests any field;
an `ex-info` without a `:rule`; a rule with no example; a message that drops its fix; types compared by name; a type
argument that only implements `ParsedValue` accepted; a sink, a child field or a `match` with no stale-type check;
the class-name claim made only when a `defparsed` loads; a claim without its role; `pt/parsed` ignoring the proof; the
`->Name` factory kept; the proof in a value's `seq`; a field named `parse-proof` allowed; `defparsed-type` or
`defdraft-type` public; no checks when a `defparsed` loads; parent checks seeing children built with the token; the
issued root holding children built without it; `empty`
returning `{}`.

Each of these broken copies of earlier versions failed a test that is still in the suite: fetches grouped by draft
type; an external check waiting only on its fetches; a truthy return counted as a pass; a tree fetching after a pure
check failed; tuples not deduplicated; a missing fetch answer allowed; `draft-of` leaving a child parsed; checks or
fetches seeing children as drafts or as open maps; `:blocked-by` naming one child; `:failed-kinds` missing a kind; a
grandchild failure not holding the grandparent; an unknown key returning
`nil`; a literal `pt/draft` leaving a field out; reserved check names allowed; a map-type method calling a function a
field can shadow; the depth limit off or missing; `match` binding a value `parse` didn't make.

## Lint

The library ships a clj-kondo config. `defparsed` expands into the functions it compiles to, so a check that uses a
field it didn't name is an unresolved symbol in the editor, and one that names a field it doesn't use is an unused
binding. A field named like a `clojure.core` function is caught when compiling instead. `match` lints as a `let` per
branch. `parse-token` or `register!` named outside `parsed-types.core` is flagged as a discouraged var. Copy the
config with:

```
clj-kondo --lint "$(clojure -Spath)" --dependencies --copy-configs --skip-lint
```
