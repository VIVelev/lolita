((lambda (fact n)
   (fact fact n))
 (lambda (f n)
   (if (= n 0)
     1
     (* n (f f (- n 1)))))
 5)
