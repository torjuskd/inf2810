(define (length2 items)
  (null? items
         0
         (+ 1 (length2 (cdr items)))))

(define (map2 proc items)
  (if (null? items)
         '()
         (cons (proc (car items))
               (map2 proc (cdr items)))))

(define (filter2 proc items)
  (if (null? items)
         '()
  (cons (if (proc (car items))
            (car items)
            '())
        (filter2 proc (cdr items)))))

(define (reduce2 proc items)
  (if (null? items)
      0
      (if (null? (cdr items))
          (car items)
          (proc (car items)
            (reduce2 proc (cdr items))))))