;; 1.4 
;; Returnerer a pluss absoluttverdien av b.
;; Hvis b er positivt vil tallene legges sammen
;; og hvis b er negativt trekkes b fra a.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 1.5
;; Ben bitdiddles test for å finne ut om interpreter bruker normal- eller applicative-order evaluation.
;; Testes med (test 0 (p))
;; Applicative-order vil si at alle argumenter evalueres før de "sendes" til prosedyren.
;; Vil derfor ende med en evig loop hvis applicative-order benyttes.
;; Ved normal-order evalueres argumentene først når vi trenger dem.
;; Prosedyren vil derfor returnere 0 med normal-order evaluation.
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; 1.6
;; Trenger if å være en special form?
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Forsøker å skrive om square-root program
;; Fører til at ethvert kall gir et rekursivt kall,
;; slik at den ikke vil terminere.
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (define (square y) (* y y))
  (< (abs (- (square guess) x)) 0.001))

;; Original sqrt-iter prosedyre
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (square-root x)
  (sqrt-iter 1.0 x))

;; 1.7
;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (inc (+ (dec a) b))))
;;(+ 4 5)
;;(inc (+ 3 5))
;;(inc (inc (+ 2 5)))
;;(inc (inc (inc (+ 1 5))))
;;(inc (inc (inc (inc (+ 0 5)))))
;;(inc (inc (inc (inc 5))))
;;(inc (inc (inc 6)))
;;(inc (inc 7))
;;(inc 8)
;;9

;;(inc 3
;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (+ (dec a) (inc b))))

;;(+ 4 5)
;;(+ (dec 4) (inc 5))
;;(+ (dec 3) (inc 6))
;;(+ (dec 2) (inc 7))
;;(+ (dec 1) (inc 8))
;;(+ 0 (inc 8))
;;9

