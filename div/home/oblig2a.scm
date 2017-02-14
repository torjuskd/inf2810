;; Oblig 2a
;; Gruppemedlemmer: Anders Jakob Sivesind og Torjus Dahle

;; 1 - Diverse
;; (a)
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car x y)
  (lambda () x))
(define (p-cdr x y)
  (lambda () y))
