(ns hooks.parsed-types-internal
  "How clj-kondo reads the macros parsed-types.core uses only inside itself.")

(defmacro defclass-once
  "The class definition itself: the guard only skips it when the class is already loaded."
  [form]
  form)
