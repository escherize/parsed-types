# Guarantees, limits and design

Every guarantee below is pinned by a named test, and every test was checked against a broken copy of the library.
The README covers how to use the library; this page covers what it promises and why.

## Guarantees

| Guarantee | Mechanism | Tested by |
|---|---|---|
| `pt/parsed` accepts only values `parse` made | `parse` records each value in a weak identity map, compared with `identical?` | `parse-issues-a-new-equal-value-and-nothing-else-is-parsed` |
| A parsed value means its draft type's checks passed | One draft type may parse to each parsed type | `one-draft-type-per-parsed-type-test` |
| A parsed value can't change | `assoc`, `dissoc`, `with-meta` and every `java.util.Map` mutator throw | `a-parsed-value-cannot-be-changed` |
| A misspelled key fails, never reads `nil` | Drafts, parsed values and the children a parent check sees have fixed fields; `pt/let` and literal `pt/draft` check keys when compiling | `unknown-keys-test`, `a-parent-check-cannot-read-a-key-a-child-lacks-test`, `let-compile-errors-test`, `let-checks-every-binding-test`, `draft-test` |
| Any field name works, except special forms | The map type's methods call every function qualified, so no field shadows one | `fields-named-like-core-functions-test`, `special-form-field-names-test` |
| The checks see the values that are issued | Each field is read once, from the type's own field; each node's value is built once and is both what parent checks see and what `parse` returns | `fields-are-read-once-from-the-record-test`, `parse-matches-the-tree-model` |
| `parse` reports exactly what the declaration says | | `parse-matches-the-model`, `parse-matches-the-tree-model`: each against a model written by hand, down to every path, `:blocked-by`, `:blocked` and status |
| Remembered passes never change an outcome | `assoc` clears the passes of the checks that read the field; passes are tied to the declaration that set them; a child field's shape check always runs | `remembered-passes-never-change-the-outcome`, `remembered-passes-never-change-a-tree-outcome-under-edits`, `passes-are-not-read-under-a-replaced-declaration`, `re-evaluating-a-child-type-test` |
| Malformed input never reaches a fetch or an external check, anywhere in a tree | Fetches and external checks wait on every pure check in the tree | `nothing-touches-the-world-until-every-pure-check-passes`, `nothing-in-a-failing-tree-fetches` |
| Each fetch runs once per declaration, with each tuple once, and answers every tuple | Tuples gathered across the call and deduplicated; a missing answer is a 500 | `parse-all-fetches-once-with-each-distinct-tuple`, `fetches-see-checked-values-and-run-once`, `payroll-test`, `a-fetch-must-answer-every-tuple-test` |
| `parse-all` agrees with `parse` | Fetches grouped by declaration | `parse-all-agrees-with-parse-across-declarations`, `parse-all-agrees-with-parse-on-trees` |
| A parsed tree is parsed all the way down, and `draft-of` returns drafts all the way down | Values built bottom-up | `parse-matches-the-tree-model`, `draft-of-returns-drafts-all-the-way-down` |
| `match` handles every member of a sum, and only parsed values reach a branch | `match` compares its clauses with the sum when it expands, and calls `pt/parsed` in each branch | `match-compile-errors-test`, `match-runs-only-on-parsed-members` |
| A deep or huge tree is refused with a 400, not a crash | The tree is walked with an explicit stack; at most 1000 levels counting the root, and 100,000 drafts counting repeats | `tree-limits-test`, `depth-boundary-test` |
| A check reads only the fields it names | `defparsed` compiles each check as a fn of its named fields, and scans its body for the others | `compile-errors-test` |
| Threads that first use a declaration at once keep their remembered passes | The first plan stored wins | `concurrent-first-use-test` |

## Errors caught when compiling

`defparsed` refuses:

- a check that reads a field the draft doesn't have, names one twice, or uses one it didn't name;
- a check or fetch name that isn't a qualified keyword, is used twice, or is in `parsed-types.child`, which is
  reserved for the checks on child fields;
- a check with no doc string or no body;
- a fetch binding that names no declared fetch, binds a local with a field's name, or binds two fetches to one local;
- a pure check that binds a fetch;
- an option other than `:status`, or a `:status` that isn't an integer;
- a field named twice or like a special form, a field declared as a child twice, or a child field that isn't a
  field.

`pt/draft` refuses a literal map that names a key that isn't a field, or leaves a field out. `pt/let` refuses a key
that isn't a field of the type (of every member, for a sum), `:or`, `:strs`, `:syms`, qualified keys, and vector
destructuring. `match` refuses a missing member, a type that isn't a member, and a member handled twice.

## Errors caught when a declaration is first used

`parse` throws a 500 when:

- `:parses-to` isn't a parsed type, its fields differ from the draft's, or another draft type already parses to it;
- a check's or fetch's `:f` isn't a fn, or a pure check's `:f` closes over a local (this refuses `constantly`,
  `partial`, `comp` and `memoize`);
- `:reads` isn't a sequence of distinct fields of the draft;
- there are more than 64 pure checks, counting one per child field.

## Behavior worth knowing

- **Reading unknown keys.** A one-argument lookup throws: `(:k x)`, `(get x :k)`, `(x :k)` and destructuring. With a
  default, `(get x :k ::none)` returns it. `contains?` is false, `find` is nil, and `select-keys` leaves the key out.
- **Equality.** Drafts, parsed values and plain maps with the same entries are `=` and hash the same, so a set of
  parsed values doesn't tell a forged one apart. Proof is `pt/parsed`, never `=`.
- **No-op changes.** `(merge p {})` and `(conj p nil)` return `p` itself, which stays parsed.
- **`pt/draft` with a map built at run time.** Missing fields are nil, and a key that isn't a field is a 400: the map
  is usually a request.
- **`check` with some names.** Children run all their pure checks. A tree fetches and runs external checks only when
  every pure check in it ran and passed, so a child's external checks may not run.
- **Status in a tree.** The first pure failure decides it, children before parents; with none, the first external
  failure, children before parents.
- **Exceptions.** An exception thrown by a check or fetch, such as an existing permission check that throws a 403,
  propagates out of `parse` and stops `parse-all` at that draft.

## REPL reloads

Re-evaluating a `defparsed` makes new classes.

- A draft of the old class still parses, under the declaration it now reads.
- A parent declared before a child type was re-evaluated refuses drafts of the child, old and new, with a 400 from its
  child-field check. Re-evaluate the parent.
- A `defsum` and every compiled `(pt/parsed T ...)`, `pt/let` and `match` still name the old class, so they refuse
  values of the new one until their namespaces are reloaded.

## Design decisions

**Drafts and parsed values are map types with fixed fields, not records.** `assoc` on a record returns another record
of the same class, which for a parsed value would look like proof, and `map->` turns a typo into an extra key.

**Proof is identity, not a token.** A secret in a field or in metadata can be copied from one parsed value into a
forged one. An identity map holds nothing a caller can copy.

**One draft type per parsed type.** If two draft types could parse to `Transfer`, a `Transfer` would mean only that
some checks passed.

**A check's argument vector is its reads.** One list says what the check reads, and the compiler holds the body to
it.

**Checks happen in ordinary forms.** `pt/let` is `let` and `pt/draft` builds what `->TransferDraft` builds. Each adds
a compile-time check and expands to the plain form.

**A declaration is still data.** `defparsed` writes a plain map, which `(pt/declaration draft)` returns. Declarations
can be written by hand with `pt/defparsed-type` and `pt/defdraft-type`.

**Only pure checks must close over nothing.** A pure check's pass is remembered, so it must depend only on its
arguments. An external check or a fetch runs on every call, so it may hold a database handle.

**A check passes only on exactly `true`.** `nil`, a string or a map of errors is a failure, so a check can't pass by
accident.

**Children are drafts, parsed with the parent.** All the tree's checks run at one moment, all its fetches are batched,
and a child parsed elsewhere, perhaps for another user, can't be reused.

## Tests

`clojure -X:test` runs:

- `bank-test`: transfers, payrolls, budgets and payments, against an in-memory bank;
- `core-test`: each behavior and declaration error, and the ways to forge or change a value we know of;
- `defparsed-test`: compile errors, unknown keys, field names, REPL reloads, concurrent first use;
- `properties-test`: `test.check` properties over random drafts, edits and declarations, against a model written by
  hand;
- `tree-properties-test`: `test.check` properties over random trees with `:one` and `:many` children, malformed
  children and edits to child fields, against a model of the whole failure map.

Each of these deliberately broken copies of the library fails at least one test: fetches grouped by draft type;
an external check waiting only on its fetches; a truthy return counted as a pass; a tree fetching after a pure check
failed; passes read under a replaced declaration; tuples not deduplicated; a missing fetch answer allowed; `draft-of`
forgetting passes or leaving a child parsed; a parsed parent holding a raw child; checks or fetches seeing children as
drafts or as open maps; `:blocked-by` naming one child; the status taken root-first; a grandchild failure not holding
the grandparent; fields read through `get`; two draft types for one parsed type; `check` on a subset letting a tree
fetch; a child-field check remembered; racing threads keeping different plans; an unknown key returning `nil`; a
literal `pt/draft` leaving a field out; `pt/let` not checking keys, or checking only its first binding; `pt/let`
treating a local as `pt/parsed`; reserved check names allowed; a map-type method calling a function a field can
shadow; the depth limit off or missing; `match` binding a value `parse` didn't make.

## Lint

The library ships a clj-kondo config. `pt/let` lints as `let`. `defparsed` expands into the functions it compiles to,
so a check that uses a field it didn't name is an unresolved symbol in the editor, and one that names a field it
doesn't use is an unused binding. A field named like a `clojure.core` function is caught when compiling instead.
`match` lints as a `let` per branch. Copy the config with:

```
clj-kondo --lint "$(clojure -Spath)" --dependencies --copy-configs --skip-lint
```
