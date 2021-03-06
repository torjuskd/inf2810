;; 1 Par og lister
(display "1. Par og lister")(newline)

;;Boks-og-peker-diagrammer
;;(a)(cons 47 11)
;; ------------     ------------
;; |    ||    | --> |    ||  / |
;; ------------     ------------
;;   |                 |
;;   v                 v
;;  47                 11

;;(b)(cons 47 '())
;; ------------
;; |    ||  / |
;; ------------
;;   |
;;   v
;;  47

;;(c)(list 47 11)
;; ------------     ------------
;; |    ||    | --> |    ||  / |
;; ------------     ------------
;;   |                 |
;;   v                 v
;;  47                 11

;;(d) '(47 (11 12))
;; ------------     ------------
;; |    ||    | --> |    ||  / |
;; ------------     ------------
;;   |                 |
;;   v                 |
;;  47                 v
;;                  ------------     ------------
;;                  |    ||    | --> |    ||  / |
;;                  ------------     ------------
;;                     |                 |
;;                     v                 v
;;                    11                12

;;(e)(define foo '(1 2 3))
;;   (cons foo foo)

;; ------------     ------------     ------------     ------------     
;; |    ||    | --> |    ||    | --> |    ||    | --> |    ||  / |
;; ------------     ------------     ------------     ------------     
;;   |                 |               |                 |
;;   |                 v               v                 v
;;   |                 1               2                 3
;;   v
;; ------------     ------------     ------------
;; |    ||    | --> |    ||    | --> |    ||  / |
;; ------------     ------------     ------------
;;   |                 |               |
;;   v                 v               v
;;   1                 2               3

(display "Trekke ut elementet '3' fra lister: ")
(newline)
;; Trekke ut elementet '3' fra følgende lister
;;(f)(1 2 3 4)
(car (cdr (cdr '(1 2 3 4))))

;;(g)((1 2) (3 4))
(car (car (cdr '((1 2) (3 4)))))

;;(h)((1) (2) (3) (4))
(car (car (cdr (cdr '((1) (2) (3) (4))))))

(display "Lage liste: \"((1 2) (3 4))\" på forskjellige måter:")
(newline)
;;(i) Lage liste i deloppgave (g), bare ved bruk av cons:
(cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) '()))
;;(i) Lage liste i deloppgave (g), bare ved bruk av list:
(list (list 1 2) (list 3 4))

;; 2 Rekursjon over lister og høyereordens prosedyrer
(newline)(display "2. Rekursjon over lister og høyereordens prosedyrer")(newline)

;; Halerekursiv length-metode
;;(a)
(define (length2 items)
  (define (iter items count)
    (if (null? items)
        count
        (iter (cdr items) (+ 1 count))))
  (iter items 0))

;; (Slik kunne den vært skrevet ikke-halerekursivt)
(define (length3 items)
  (null? items
         0
         (+ 1 (length2 (cdr items)))))

;;(b)
;; Prosedyre som reverserer en liste:
(define (rev-list-master items)
  (define (iter items1 items2)
    (if (null? items1) items2
        (iter (cdr items1) (cons (car items1) items2))))
  (iter items '()))
;; Her brukes (spesialtilfellet) halerekursjon.
;; Det er rett og slett lettere å bygge opp listen bakfra
;; og forover på denne måten; vi trenger bare å legge et og et
;; element til listen.
;; Hadde vi brukt "vanlig" rekursjon, måtte vi først traversert
;; hele listen en gang, for så å gå tilbake omvendt vei.

;; Det kunne vært gjort på "vanlig" rekursiv måte,
;; men det hele blir veldig kronglete ettersom det er
;; vanskeligere å få ut listen i riktig format.
;; Kan gjøres slik: (men det er ikke elegant)
(define (rev-list-rec items)
  (define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))
  (define (rev items)
  (if (null? (cdr items))
      (car items)
          (list (rev (cdr items)) (car items))))
  (flatten (rev items)))


;;(c) Returnere liste der alle forekomster av gitt tall er fjernet:
(define (ditch x items) 
 (filter (lambda (x) (if (= 3 x) #f #t)) items))

(define (filter proc items)
  (if (null? items)
         '()
   (cond ((proc (car items)) (cons (car items) (filter proc (cdr items))))
         ((pair? items) (filter proc (cdr items)))
         (else '()))))

;; Prosedyren ditch benytter seg av hjelpeprosedyren filter.
;; Det interessante med prosedyren over er at vi har først et
;; ikke-hale rekursivt kall, "(cons (car items) (filter proc (cdr items)))"
;; og ved et annet tilfelle ville vi få et halerekursivt kall: "(filter proc (cdr items))".
;; Prosedyren kan altså ikke skrives om til en løkke/iterativ prosess.
;; Vi får dermed en "vanlig" rekursiv metode (ikke-hale rekursjon) (regner med worst-case).
;; Minnebruk vil øke betraktelig ved økende input, tidsbruken vil
;; være omtrent lik som den ville vært om metoden var halerekursiv.

;; Under kjøring kan vi se for oss at minnet utvikler seg ca. slik:
;;(ditch 3 '(1 2 3 4))
;;(filter (lambda(3) (1 2 3 4)))
;;(filter (lambda(3) (1 (filter (lambda(3) (2 3 4))))))
;;(filter (lambda(3) (1 (filter (lambda(3) (2 (filter (lambda(3) (3 4)))))))))
;;(filter (lambda(3) (1 (filter (lambda(3) (2 (filter (lambda(3) (filter (lambda(3) (4)))))))))))
;;(filter (lambda(3) (1 (filter (lambda(3) (2 (filter (lambda(3) (4)))))))))
;;(filter (lambda(3) (1 (filter (lambda(3) (2 4))))))
;;(filter (lambda(3) (1 2 4)))
;;=>(1 2 4)


;;(d)
;; Prosedyre nth, som returnerer n-te element
(define (nth n items)
  (define (iter items count)
    (cond ((null? items) '())
          ((= count n) (car items))
          (else (iter (cdr items) (+ 1 count)))))
  (iter items 0))

;;(e)
;; Returnerer indeks til første forekomst av input tall i liste,
;; eller #f om listen ikke inneholder tallet.
(define (where n items)
  (define (iter items count)
    (cond ((null? items) #f)
          ((= n (car items)) count)
          (else (iter (cdr items) (+ count 1)))))
  (iter items 0))

;;(f)
;; map med to lister
(define (map2 proc items1 items2)
  (cond ((or (null? items1) (null? items2)) '())
        (else (cons (proc (car items1) (car items2)) (map2 proc (cdr items1) (cdr items2))))))

;;(g)
;; map2 for å beregne gjennomsnitt:
(define (average items1 items2)
  (map2 (lambda (x y) (/ (+ x y) 2)) items1 items2))

;; Bruk av map2 for å returnere #t på posisjoner der begge elem. er partall,
;; ellers #f.
(define (even-numbers? items1 items2)
  (map2 (lambda (x y) (if (and (even? x) (even? y)) #t #f)) items1 items2))

;;(h)
;; returnerer prosedyre som sjekker at predikatet holder for begge argumenter:
(define (both? pred)
  (lambda (x y) (if (and (pred x) (pred y)) #t #f)))
;;eks.
;; (map2 (both? even?) '(1 2 3) '(3 4 5))
;; =>(#f #t #f)

;;(i)
;; Returnerer prosedyre som tar en prosedyre og bruker den på input "to ganger".
(define (self proc)
  (lambda (x) (proc x x)))


;; Bonus!
(define (map3 proc items)
  (if (null? items)
         '()
         (cons (proc (car items))
               (map2 proc (cdr items)))))

(define (reduce2 proc items)
  (if (null? items)
      0
      (if (null? (cdr items))
          (car items)
          (proc (car items)
            (reduce2 proc (cdr items))))))

