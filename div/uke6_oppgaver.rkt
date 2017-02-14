;; Append som kun tar to lister

;;(define (append lst1 lst2)
;;  (if (null? lst1)
;;      lst2
;;      (cons (car lst1) (append (cdr lst1) lst2))))

;; Generalisere append til å ta et vilkårlig antall elementer
;;(define (append . lists)
;;  (cond ((null? lists) '())
;;        ((null? (car lists)) (apply append (cdr lists)))
;;        (else (cons (caar lists)
;;                    (apply append (cdar lists) (cdr lists))))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;;quicksort
(define (quicksort lst)
  (cond ((null? lst) '())
        ((= (length lst) 1) lst)
        (else (append (quicksort (filter (lambda (x) (<=  x (car lst))) (cdr lst)))
                      (list (car lst))
                      (quicksort (filter (lambda (x) (> x (car lst))) (cdr lst)))))))
        