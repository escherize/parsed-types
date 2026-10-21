# Guarantees, limits and errors

This page lists what parsed-types promises, the test that pins each promise, its limits, and every error rule. The [README](../README.md) covers how to use the library and why it is built this way.

## Guarantees

| Guarantee | Mechanism | Tested by |
|---|---|---|
| `pt/parsed` accepts only values `parse` made | A parsed value has a hidden `parse-proof` field. `parse` sets it to a private token. `pt/parsed` first checks that the value's class is one `defparsed` registered, by identity, then asks the value `(issuedWith token)`, which compares inside the type | `parse-issues-a-new-equal-value-and-nothing-else-is-parsed`, `parsed-refuses-anything-parse-did-not-issue-test` |
| The token never leaves the value | The field is `volatile-mutable`, which Clojure compiles to a non-public field that interop can't read, and nothing sets it after the constructor. No public method returns it. A hand-written class is refused before it is asked, so it never sees the token | `the-proof-never-leaves-the-value-test` |
| A hand-written type is refused, even with the same name | `pt/parsed` requires the class `defparsed` registered, by identity | `a-hand-written-class-with-the-same-name-is-refused-test` |
| A `defparsed` can't redefine another's class | Each class name is claimed by the namespace whose `defparsed` first defined it. A `defparsed` whose class would take a name claimed by another namespace (`my_app.bank` for `my-app.bank`) throws `::bad-declaration` before its `deftype`. `defparsed` defines one class, so `Foo` and `FooDraft` are two unrelated types | `a-class-name-belongs-to-one-namespace-test`, `defparsed-defines-one-class-test` |
| The type has no positional constructor fn | `defparsed` removes `->Name`. `pt/parse` takes a map that names every field, so reordering a type's fields swaps no value. `(Debit. ...)` compiles, and `pt/parsed` refuses what it builds | `the-positional-factories-are-removed-test`, `parsed-refuses-anything-parse-did-not-issue-test` |
| The map to parse names every field, and only fields | A literal map in `(pt/parse T {...})` is checked when the call compiles, through `parse`'s `:inline`; any other map, and every map of `parse-all`, when it runs. A missing or unknown key throws `::bad-input` with the nearest field, so a misspelled key never parses as nil | `parse-checks-a-literal-map-when-it-compiles-test`, `parse-checks-any-map-when-it-runs-test`, `parse-all-checks-each-map-test` |
| The proof is not a field | `parse-proof` isn't in what `pt/parsed` returns, equality, the hash or the printed form. A field may not be named `parse-proof` | `the-proof-is-not-a-key-test`, `field-name-limits-test` |
| A parsed value means its type's checks passed | Only `defparsed` defines types, with their checks. `defparsed-type` is private, and `parse` takes only a type `defparsed` registered, never a sum | `only-defparsed-makes-types-test`, `parse-takes-a-parsed-type-test` |
| A declaration error is found when the `defparsed` loads, at the latest, and a refused one changes nothing | The macro refuses what it can see when it expands. The expansion is a top-level `do`, which runs each form before it compiles the next; its first form checks the fetch fns before any class is defined | `compile-errors-test`, `declaration-errors-at-load-test` |
| A type whose `defparsed` stopped after its `deftype`s says so | Its classes are claimed but have no plan, so every use of them throws `type-not-loaded`, saying to fix the error and re-evaluate the `defparsed` | `declaration-errors-at-load-test` |
| `defparsed` works only at the top level, and says so | Inside another form, the whole form compiles before any of it runs, so the classes would exist before the checks that guard them. `defparsed` throws when it has locals (`&env`) or compiles inside a fn body (`Compiler/METHOD` is bound) | `defparsed-only-at-the-top-level-test` |
| A parsed value never equals a plain map, or a value built without the proof | The type's equality checks the class and the proof; it isn't marked map-equivalent, so Clojure's maps refuse it too. So `(contains? #{real} forged)` is false | `parse-issues-a-new-equal-value-and-nothing-else-is-parsed`, `parse-test`, `the-proof-is-not-a-key-test` |
| A parsed value holds only immutable values | `parse` walks every field of the input, deeply, and accepts only the values listed under "Accepted field values". Anything else throws `::bad-input` with the field, the path inside it, the class and the fix | `mutable-field-values-are-refused-test` |
| A field holds only parsed values that `parse` issued | The walk refuses a value of a parsed type that `parse` did not issue (`unissued-in-field`). A plain map in a field is data: a type that wants a parsed value there says so in a pure check, with `pt/parsed?` | `a-field-holds-parsed-values-test` |
| An issued value in a field is not walked again | It proved its own checks and holds only immutable values, so the walk stops at it | `an-issued-value-in-a-field-is-not-walked-again-test` |
| A clock is refused, though its class is a final `java.time` class | `Clock` and `InstantSource` are checked before the `java.time` rule | `a-clock-is-refused-test` |
| The walk can't overflow the stack, or skip the depth limit through shared structure | The walk keeps an explicit stack, and records the height of each collection it walked: the most levels below it, a leaf inside counting one. One reached again is skipped when its deepest value is still within the limit from there, and walked again otherwise | `the-walk-keeps-its-own-stack-test`, `shared-structure-is-walked-again-when-reached-deeper-test`, `a-shared-collection-of-leaves-counts-its-leaves-test` |
| The walk costs each collection's size once, however many depths it is reached at | The same heights: a 100,000-element vector reached at each of 990 levels is walked once | `a-collection-reached-at-many-depths-is-walked-once-test`, `shared-structure-is-walked-once-test`, `a-shared-collection-of-leaves-is-walked-once-test` |
| A parsed value can't change | `assoc`, `conj`, `with-meta` and `empty` throw `::cannot-change`. The type is not a `java.util.Map` or an `IPersistentMap`, so `dissoc` and Java mutators don't apply to it | `a-parsed-value-cannot-be-changed-test`, `a-parsed-value-cannot-be-changed`, `not-a-map-test` |
| A walk over a parsed value throws, and never drops the proof | `empty` on a parsed value throws `::cannot-change`, so `clojure.walk` can't rebuild it as a plain map | `empty-throws-test`, `not-a-map-test` |
| A parsed value is read only through `pt/parsed` | Every Clojure read, as `(:k x)`, `get`, destructuring, `contains?`, `count` or `seq`, throws `::must-unwrap`, with the `pt/parsed` call to write. So a sink that skips `pt/parsed` throws on the first parsed value | `every-read-throws-must-unwrap-test`, `a-sink-that-skips-parsed-fails-on-real-values-test`, `parsed-returns-a-plain-map-test` |
| A field may be named like any `clojure.core` function | The types' methods call every function qualified, so no field shadows one | `fields-named-like-core-functions-test`, `a-field-cannot-replace-the-checks-test` |
| Equality is a record's: same type, each field `=`, and for a parsed value the same proof | `equiv` checks the class and the proof, then compares fields with `=`, so `1.0M` equals `1.00M`. The hash reads only the fields | `equality-test`, `the-proof-is-not-a-key-test` |
| A value prints with its type, never its proof | One `print-method` and one `pprint` dispatch on each library interface print `#ns.Type{...}`; `str` and `toString` return the same. A `defparsed` adds no method | `parse-test` |
| `parse` reports exactly what the declaration says | | `parse-matches-the-model`: against a model written by hand, down to which check fails first and what it returned |
| Checks run in the order written, and the first failure stops the parse | Pure checks run in the order written, then the fetches, then external checks in the order written. The first check that returns anything but `true`, or throws, stops the parse: no check after it runs, so nothing runs after a failed permission check. `defparsed` refuses a pure check written before one on a proper subset of its fields, so the check that fails first names the root cause | `pure-checks-run-in-the-order-written-test`, `order-test`, `transfer-test`, `parse-matches-the-model` |
| A check body that throws stops the parse as a failure does | Each `Exception` a pure or external check's body throws is reported as that check's failure: `:threw` holds it, and it is the cause. An exception the library throws inside a check body, such as `::must-unwrap` for a read of a parsed value without `pt/parsed`, is that check's `:threw` too. An exception from a fetch or from the library outside a check body, an `Error`, and an `InterruptedException`, with the thread's interrupt flag set again, propagate | `a-check-that-throws-stops-the-parse-test`, `what-a-check-throws-that-is-not-recorded-test` |
| Malformed input never reaches a fetch or an external check | A pure check that fails stops the parse before any fetch. `parse-all` runs every map's pure checks before any fetch, so one malformed map fetches nothing for the batch | `nothing-touches-the-world-until-every-pure-check-passes`, `order-test`, `batching-test` |
| Each fetch runs once per declaration, with each tuple once, and answers every tuple | `parse-all` gathers the tuples of every map in the call, per fetch, and deduplicates them; a missing answer throws `::bad-fetch`. So a parent built from `(pt/parse-all Part parts)` costs one call per fetch, not one per part | `parse-all-fetches-once-with-each-distinct-tuple`, `batching-test`, `payroll-test`, `a-fetch-must-answer-every-tuple-test` |
| `parse-all` agrees with `parse` | Each map's checks see only that map, and each declaration calls its own fetch | `parse-all-agrees-with-parse`, `each-declaration-uses-its-own-fetch` |
| A fetch may be a var | A var that holds a fn passes the load-time check and is called through, so `with-redefs` on it reaches the fetch | `a-fetch-may-be-a-var-test` |
| `match` handles every member of a sum, and only parsed values reach a branch | `match` compares its clauses with the sum when it expands, and runs a branch only when the value is one `parse` issued, of that member's class | `match-compile-errors-test`, `match-runs-only-on-parsed-members` |
| A check reads only the fields it names | `defparsed` compiles each check as a fn of its named fields, and refuses any symbol in its body equal to a field the argument vector doesn't name, quoted or bound as a local too. For a symbol that also names a var, as `count` does, the message offers both fixes: add the field, or write `clojure.core/count` | `compile-errors-test`, `every-suggested-fix-applies-test` |
| Every error names what happened, the rule it broke and the fix | Each `ex-info` carries a `:rule` in `parsed-types.rule` next to `:type`, and its message holds the values, the rule and the fix as code. A misspelled key suggests the nearest field | `every-error-names-what-happened-the-rule-and-the-fix-test`, `every-rule-has-an-example-test`, `every-throw-names-a-rule-test`, `no-other-throws-test`, `suggestions-are-data-too-test` |
| Only types `defparsed` defined count as types | `pt/parsed`, `pt/parsed?` and `defsum` accept a class only when `defparsed` registered it, by identity. A class that only implements `ParsedValue` is refused with `::bad-input` | `the-type-argument-is-checked-test`, `only-sum-builds-a-sum-test` |
| A sum is built only from distinct parsed types | A sum isn't a map, so `assoc` can't change one. Every use checks its members, so one built with its constructor is checked too. Sums are equal when their names and members are | `only-sum-builds-a-sum-test` |
| Reloading the library keeps every type and value defined before | The token is a `defonce`, and `ParsedValue` and `Sum` are defined only when they are not loaded yet | `reloading-the-library-keeps-every-type-test` |
| Types compare by class, like records, and a stale class says what to re-evaluate | `pt/parsed`, `parsed?`, sums and `match` compare classes by identity. Where the class differs but the name matches, they throw `::stale-type`, naming the `defsum`, the namespace a `match` was compiled in, or the code that built the old value | `re-evaluating-a-type-test`, `re-evaluating-a-member-of-a-sum-test`, `every-reload-command-applies-test` |
| The public API is eight names | Every fn that only generated code calls is private | `the-public-api-is-what-users-call-test` |

## Accepted limits

The proof stops accidents, not forgers. These ways around it, and these gaps, are accepted:

- **Reflection can forge:** `setAccessible` reaches the proof field.
- **Any private var of `parsed-types.core` can forge.** Code can name the private token as
  `@#'parsed-types.core/parse-token` and pass it to the constructor. `#'pt/issue` issues a value from any fields,
  and `#'pt/register!` records a hand-made plan. `ns-resolve`, `ns-interns` and `find-var` reach them without naming
  them. The clj-kondo config this library ships flags every private var named outside `parsed-types.core`, by `#'`
  or `var`; it can't see `ns-resolve`, `ns-interns` or `find-var`.
- **`with-redefs` on library fns.** Rebinding `pt/parsed?` or a private fn changes what the library checks.
- **Reader syntax fails closed.** `#ns.Debit[...]` calls the constructor, so it builds a value without the proof.
  `pt/parsed` refuses it.
- **The walk trusts a class's interfaces.** A type that implements `IPersistentVector` mutably passes.
- **`vec` of a Java array can share it.** `(vec a)` of 32 or fewer elements wraps the array itself, and the walk
  can't tell that vector from another. Use `(into [] a)`, which copies; the message for an array names it.
- **A sum can be built with its constructor.** `(parsed_types.core.Sum. 'S members)` builds one, so every use
  checks the members again. One built that way with real members is the same as one `defsum` builds.
- **`in-ns` can redefine a namespace's types**, with whatever checks it writes, as a REPL reload does.
- **Replaced classes are never released.** The registry keeps each class a REPL re-evaluation replaces, with its
  plan, so a REPL session grows by two classes per re-evaluation.
- **A pure check can call the world.** The library can't stop its body from calling a database.
- **External checks can go stale.** An external check reads the world at the moment of `parse`. A balance that
  passed can change later, so parse right before you act.
- **An external check gets nil when a fetch found nothing.** A fetch returns nil for a tuple with no row, and every
  external check bound to that fetch gets the nil. Test for it in the first check written over that fetch, as in
  `(some? row)`: when it fails, no later check runs. A check that fails while every fetch it binds found nothing
  says so in its message.

## Errors

Every error the library throws is an `ex-info` whose ex-data has a `:type`, a keyword in `parsed-types.core`, and a
`:rule`, a keyword in `parsed-types.rule` that names the rule it broke, such as `:parsed-types.rule/got-unparsed`. Its
message says, in this order, what happened with the concrete values (the type, the field or key, the path, the class
it got), the rule in one clause, and the fix, as code where it can:

```
Expected a parsed Debit, got a plain map, which is not checked. A sink takes only a value pt/parse issued. Parse it,
as in (pt/parse Debit {:user-id ..., :account-id ..., :amount ...}), and pass the result.
```

The ex-data holds the same facts as data. When a key is not a field, the message suggests the nearest field, as in
`Did you mean :account-id?`, and `:suggestion` holds it. `parsed-types.errors-test` has an example of every
rule, and fails when `core.clj` has a rule with no example, an `ex-info` without a `:rule`, or a `throw` of anything
but an `ex-info`. Class names in ex-data are strings, so ex-data can be serialized, except for the exception `:threw`
holds.

| `:type` | Thrown when | Other ex-data |
|---|---|---|
| `::invalid` | `parse` or `parse-all` ran a check that failed or threw, and stopped there. The message names the check, its doc and what it returned or threw, and for a result that is truthy but not `true`, says to wrap the body in `(boolean ...)` | `:check`, `:doc`, `:kind`, `:returned` or `:threw`, `:parsed`, and `:index` from `parse-all` |
| `::not-parsed` | `pt/parsed` or `match` got a value `parse` didn't issue | `:expected`, `:got` |
| `::stale-type` | a class doesn't match but its name does: one side is from before a REPL re-evaluated a `defparsed` | `:class`, and `:ns` where it applies |
| `::must-unwrap` | a read of a parsed value without `pt/parsed`: `(:k x)`, `(x :k)`, `get`, destructuring, `contains?`, `find`, `count` or `seq`. The message is the `pt/parsed` call to write | `:parsed`, `:op`, `:key` when a key was read |
| `::cannot-change` | `assoc`, `conj`, `with-meta` or `empty` on a parsed value | `:parsed`, `:op`, `:key` |
| `::no-field` | `assoc` of a key that isn't a field. It is reported before the change is refused, so the fix names a field | `:parsed`, `:key`, `:fields`, `:suggestion` when a field is close |
| `::bad-input` | the map to parse is not a map, is already parsed, or has a missing or unknown key; a field holds a value that is not known to be immutable, or an unissued value; a library fn got the wrong kind of argument | `:parsed`, `:index` from `parse-all`; `:unknown`, `:missing`, `:fields`, `:suggestion` for keys; `:field`, `:path`, `:got` for a field's value |
| `::bad-declaration` | a declaration error, when `defparsed`, `defsum` or `match` expands or loads, or a type whose `defparsed` stopped after its `deftype` | `:parsed` when found at load; `:class` and `:ns` for `type-not-loaded` |
| `::bad-fetch` | a fetch returned no entry for some tuples, or no map | `:fetch`, `:missing` |

For `::invalid`:

- `:check` and `:doc` are the name and doc of the check that failed. It is the first that did not return exactly
  `true`, in the order the checks ran.
- `:kind` is `:pure` or `:external`. A pure failure means the input is malformed. An external one means a check of
  the world refused it.
- `:returned` is what the check returned. A check whose body threw has `:threw`, the exception, instead, which is
  also the cause. A check that threw has a bug; the input may be fine.
- `:parsed` is the name of the parsed type's class. From `parse-all`, `:index` is the index of its map.

### Every rule

Reading values (`::must-unwrap`):

- `read-through-parsed`: a read of a parsed value. Fix: `(let [{:keys [...]} (pt/parsed T x)] ...)`.

Changing values (`::cannot-change`, `::no-field`, `::bad-input`):

- `parsed-value-is-final`: a change to a parsed value. Fix: `(pt/parse T (assoc (pt/parsed T x) :k v))`.
- `no-metadata`: `with-meta`. Fix: keep it beside, as in `{:value x, :meta m}`.
- `no-empty`: `empty`. Fix: `(pt/parsed T x)` for a plain map of the fields.
- `no-such-field-assoc`: `assoc` of a key that isn't a field. Fix: one of its fields, as named.
- `not-a-library-value`: a private fn the types call got another value.

Field names (`::bad-declaration`):

- `fields-not-symbols`: the fields aren't a vector of simple symbols.
- `field-named-twice`: a field, or a field a check reads, is named twice.
- `field-name-unusable`: `/`, a special form, a dot, two names with one Java name, `__meta` and the other names
  `deftype` reserves, or `parse-proof`. Fix: rename it, as the message shows.

Declarations (`::bad-declaration`):

- `name-not-symbol`: a type or sum named by anything but a symbol, in `defparsed`, `defsum` or `match`.
- `name-with-dot`: a type name with a dot.
- `missing-argument`: `defparsed`, `defsum` or `match` called with one argument.
- `unknown-clause`: a clause other than `pure`, `external` or `fetch`.
- `check-options`: an options map on a check.
- `name-not-qualified`: a check or fetch name that isn't a qualified keyword.
- `check-no-doc`, `check-no-body`, `check-no-field-vector`: a check without its doc, body or argument vector.
- `pure-check-fetches`: a pure check that binds a fetch.
- `reads-unknown-field`: a check or fetch that reads a field the type doesn't have.
- `check-bad-fetch-binding`: a fetch binding that names no declared fetch.
- `fetch-local-taken`: two fetches bound to one local, or a fetch bound to a field's name.
- `check-uses-unnamed-field`: a check body that uses a field its argument vector doesn't name.
- `fetch-no-fn`: a fetch with no function.
- `name-used-twice`: a check or fetch name used twice.
- `pure-check-order`: a pure check written before one on a proper subset of its fields.
- `defparsed-not-top-level`: a `defparsed` inside another form.
- `class-name-taken`: a class name another namespace claimed.
- `fetch-not-fn`: a fetch that is not a fn or a var holding one, found when it loads.
- `type-not-loaded`: a type whose `defparsed` stopped after its `deftype`.
- `bad-sum-members`: a `defsum` whose members aren't distinct parsed types.
- `match-not-a-sum`, `match-clause-shape`, `match-not-a-type`, `match-not-a-member`, `match-missing-member`,
  `match-member-twice`: a `match` that names no sum, has a clause that isn't a type, binding and expression, names
  a type that isn't one or isn't a member, misses a member, or names one twice.

Parsing (`::bad-input`, `::invalid`, `::bad-fetch`):

- `input-not-a-map`, `already-parsed`: `parse` or `parse-all` got something other than a map, or a value `parse`
  already issued. Fix for the second: pass it as it is, or `(pt/parse T (assoc (pt/parsed T x) :k v))`.
- `input-unknown-keys`, `input-missing-keys`: the map to parse names a key that isn't a field, with the nearest
  field, or leaves a field out. Found when the call compiles for a literal map in `pt/parse`, when it runs otherwise.
- `parse-all-not-a-vector`: `parse-all` got a map, or something not sequential.
- `mutable-value`, `url-value`, `not-known-immutable`: a field holds a mutable value, a `java.net.URL`, or a value
  of another class. See "Accepted field values".
- `value-too-deep`: a field nests deeper than 1000 levels.
- `unissued-in-field`: a field holds a value of a parsed type that `parse` didn't issue.
- `check-failed`: a check returned anything but `true`, or threw, and the parse stopped there. Fix for a pure check that threw: a pure check
  written before it that guards its input, as in `(pure ::amount-number "amount is a number." [amount] (number? amount))`.
- `fetch-missing-tuples`: a fetch returned no map, or no entry for some tuple.

Sinks (`::bad-input`, `::not-parsed`, `::stale-type`):

- `not-a-parsed-type`: `pt/parsed`, `pt/parsed?` or `pt/declaration` got a type argument that is neither a parsed
  type nor a sum, or `parse` got a sum.
- `got-unparsed`: a sink got nil, a plain map, a value built by hand, or a value of another type. Fix:
  `(pt/parse T {...})`.
- `match-compiled-before-member`: a `match` compiled before a member joined its sum.
- `stale-value`, `stale-sink`, `stale-match`: see "REPL reloads".

## When each error is caught

`defparsed`, `defsum` and `match` throw what they can see when they expand, so the file doesn't compile: every
declaration rule above but `fetch-not-fn` and `type-not-loaded`. So does `pt/parse` with a literal map whose keys
are not exactly the fields. A call to `pt/defparsed-type` or `->Name` doesn't compile either. Each reaches a `try` as a
`CompilerException`, with the `ex-info` as its cause.

The fetch fns are checked when the form loads, before its `deftype`s, so a refused re-evaluation leaves the type as
it was, and a fetch fn can't name the type being defined. A later error, such as a check body that doesn't compile,
leaves classes with no plan, which throw `type-not-loaded` until the `defparsed` is fixed and re-evaluated.

## Accepted field values

`parse` walks every field, map keys and metadata included, and accepts only these values:

- nil; `Boolean`, `Character`, `String`, keywords, symbols, `UUID` and `Pattern`;
- `Long`, `Integer`, `Short`, `Byte`, `Double`, `Float`, `BigInt`, `Ratio`, and `BigDecimal` and `BigInteger` by
  exact class, since they can be subclassed;
- `java.net.URI`, `java.util.Locale`, `java.util.Currency`, and a `java.nio.file.Path` of the default file system,
  by exact class; a `Path` of another file system is refused;
- Java enum constants, and classes;
- a `java.util.Optional`, whose value is walked;
- final classes of the `java.time` package, but not a `Clock` or `InstantSource`, which reads the time each time it
  is asked;
- values `parse` issued, which are not walked again;
- persistent vectors, maps (records included), sets, lists and queues whose contents pass. A sorted map or set must
  sort with the default comparator, since a custom one can read state that changes.

Everything else is refused. A mutable value is refused with `mutable-value` and the fix for its class:
`@a` for an atom, `(into [] a)` for an array, `(vec s)` for a lazy or other seq, since realizing one could run side
effects or never end, `(.toInstant d)` for a `java.util.Date` or `java.sql.Timestamp`, and `(.toLocalDate d)` or
`(.toLocalTime d)` for a `java.sql.Date` or `Time`, which throw on `.toInstant`. A `java.net.URL` is refused with
`url-value`, since its `equals` looks the host up over the network: use `(.toURI url)`. Any other class is refused
with `not-known-immutable`: convert it to an accepted value, as in `(str x)`.

## Behavior worth knowing

- **Java interop and printing show fields.** A `deftype` field is public, so `(.-amount ^Debit x)` reads a parsed
  value without `pt/parsed`. It names the class and the field on purpose, so it is not a mistake the
  library guards. `pr-str`, `str` and `pprint` print the fields too, for people reading logs; reading them back out
  of the printed text is not a mistake either.
- **A few reads throw a JDK exception, not `::must-unwrap`.** `nth`, sequential destructuring and `vec` throw
  `UnsupportedOperationException` or `RuntimeException`; `dissoc`, `peek` and `iterator-seq` throw
  `ClassCastException`. Each fails loudly, but without the fix in its message.
- **A parsed value is not a map.** `map?` is false, and every read throws `::must-unwrap`. `pt/parsed`
  returns a plain map of a parsed value's fields, where a misspelled key reads nil, as in any map. A plain map passed
  to a sink that skips `pt/parsed` still passes: nothing stops code from destructuring a map. `dissoc` throws a
  `ClassCastException`, since the type is not an `IPersistentMap`. There is no `java.util.Map` interop.
- **No empty version.** `empty` throws, and so does anything built on `(into (empty x) ...)`, such as
  `clojure.walk`. Use `(pt/parsed T x)` for a plain map.
- **Depth.** Nesting deeper than 1000 levels is refused (`value-too-deep`). The message and `:path` show the first 8
  steps of a longer path, and `:depth` its length.
- **Cost.** The walk costs about 25 us for 1,000 numbers in 100 vectors; an issued value in a field costs one step.
  `clojure -M:bench` prints the current numbers.
- **Fetch calls.** One call per declaration and fetch for the whole `parse-all`, with each distinct tuple once.
  A pure failure in any map stops the call before any fetch.
- **No-op changes throw too.** `(merge p {})` and `(conj p nil)` throw `::cannot-change`, like any change.
- **A literal map passed to `pt/parse` evaluates its values as Clojure evaluates any map literal:** in the order
  written for up to 8 keys, and in hash order beyond, since the reader makes a hash map of a larger literal.
- **Exceptions.** A fetch's exception propagates out of `parse` and stops `parse-all`: a database outage is not
  bad input. A check body's exception stops the parse as `::invalid`, with `:threw`.

## REPL reloads

Re-evaluating a `defparsed` makes new classes with the old names. Parsed types compare by class, like records, so
code evaluated before keeps the old classes, and values built before keep theirs. Code compiled against the old
class still parses into it, with the plan it was defined with.

Where a class doesn't match but its name does, the library throws `::stale-type`. The rule says which side is old,
and the message says what to re-evaluate:

| Rule | Where | Fix the message names |
|---|---|---|
| `stale-value` | a sink or `match` got a value built before its type was re-evaluated | parse it again, or re-run the code that built it |
| `stale-sink` | `(pt/parsed Debit x)` compiled before `Debit` was re-evaluated got a new value | re-evaluate the namespace that calls it, or `(clojure.tools.namespace.repl/refresh)` |
| `stale-match` | a `match` compiled before a member was re-evaluated got a new value, or a `match` names a member re-evaluated after its `defsum` | reload the `defsum`'s namespace, then the match's, as in `(require 'app.sum 'app.core :reload)` |

Each command names the namespaces in the order they depend on each other, and works as printed:
`every-reload-command-applies-test` runs each one. A namespace that imports a type from another keeps the old class
until its `ns` form runs again, so the message offers re-evaluating single forms only when every form involved is in
one namespace.

## Tests

`clojure -X:test` runs `bank-test` (an in-memory bank), `readme-test` (every README block), `core-test` (each
behavior, the proof, mutable values, and known ways to forge or change a value), `defparsed-test` (compile and load
errors, unknown keys, field names, and REPL reloads, applying each reload command a message prints),
`properties-test` (`test.check` properties against a model written by hand), and `errors-test` (an example of every
rule, and a check that every test this page cites exists). `clojure -M:bench` prints timings.

## Lint

The library ships a clj-kondo config, installed as the README shows. A check that uses a field it didn't name is an
unresolved symbol, and one that names a field it doesn't use is an unused binding. `match` lints as a `let` per
branch. A private var of `parsed-types.core` named outside it, as in `#'pt/issue`, is flagged, and
`the-lint-config-flags-every-private-var-test` keeps the list whole.
