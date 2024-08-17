(begin
  (defmacro (aif test-form then-form else-form)
    `((lambda (it)
        (if it ,then-form ,else-form))
      ,test-form))

  ((lambda (test)
     (aif (test)
       it
       'false-branch))
   (lambda () 'some-complex-computation 'a-truthy))
)
