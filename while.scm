(begin

  (defmacro (while cond body)
    `((lambda (f)
        (f f))
      (lambda (f)
        ((lambda (res)
           (if ,cond (f f) res))
         ,body))))
  )
