(begin

  (defmacro (aif test-form then-form else-form)
    `((lambda (it)
        (if it ,then-form ,else-form))
      ,test-form))

  (aif some-variable
    it
    other-thing)

  )
