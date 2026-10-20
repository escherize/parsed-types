(ns hooks.parsed-types
  "How clj-kondo reads `defdraft`: both types, then every check as the fn it compiles to, so a check that uses a field
  it did not name is an unresolved symbol, and one that names a field it does not use is an unused binding.")

(defmacro defdraft [draft-name _arrow parsed-name fields & clauses]
  (let [decl (symbol (str draft-name "-declaration"))]
    `(do
       (deftype ~parsed-name ~fields)
       (defrecord ~draft-name ~fields)
       (def ~decl
         [~@(for [[head & args] clauses]
              (case head
                (pure external)
                (let [[_ _ & more]  args
                      more          (if (map? (first more)) (rest more) more)
                      [argv & more] more
                      [binds body]  (if (and (= 'external head) (vector? (first more)) (next more))
                                      [(first more) (rest more)]
                                      [[] more])]
                  `(fn [~@argv ~@(take-nth 2 binds)] ~@body))

                fetch
                (let [[_ _ f] args] f)

                (child children)
                (second args)

                nil))]))))
