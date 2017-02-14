(define (prod x)
  (if (= x 0)
      0
  (+ (* x x) (prod (- x 1)))))