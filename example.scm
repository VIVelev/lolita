(defmacro (hello x)
  (quote no-hello))

(hello "Vici")

(define (fact)
  (hello x))

(begin 1 2 3)

(begin . (1 . (2 . (3 '()))))
