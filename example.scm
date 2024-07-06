(defmacro (unless x body)
  (quasiquote
    (if (unquote x)
      (unquote body)
      error)))

(unless T
  something)
