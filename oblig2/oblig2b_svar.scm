;; Løsningsforslag til oppgavesett 2b, INF2810, våren 2016.
;; erikve på ifi.uio.no

;; For løsninger på 1b, 3a og 3b, se eget vedlegg med diagrammer.


;; 1 a

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))


;; 2 a

(define (make-stack stack)
  (lambda (message . rest)
    (cond ((eq? message 'push!) 
	   (for-each (lambda (x) 
		       (set! stack (cons x stack)))
		     rest))
	  ((eq? message 'pop!) 
	   (if (not (null? stack))
	       (set! stack (cdr stack))))
	  ((eq? message 'stack) stack))))

;; Kommentarer;
;;
;; Over kunne vi med fordel brukt `case' i stedet for `cond'.
;;
;; `for-each' fungerer liknende `map' ved at den anvender en prosedyre
;; på hvert element i en liste, men den samler ikke opp resultater i
;; en ny liste; brukes kun for prosedyrer som har sideeffekter, når vi
;; ikke er interessert i returverdi.


;; 2 b

(define (stack s)
  (s 'stack))

(define (pop! s)
  (s 'pop!))

(define (push! s . rest)
  (apply s (cons 'push! rest)))


;; 3 c

;; Lineært minnebruk:
;; (define (cycle? lst)
;;   (define (iter lst seen)
;;     (cond ((null? lst) #f)
;;           ((memq lst seen) #t)
;;           (else (iter (cdr lst) (cons lst seen)))))
;;   (iter lst '()))

;; Kommentar; `memq' er en innebygd prosedyre som sjekker om et gitt
;; objekt er med i en gitt liste, med `eq?' som test for likhet. 
;; Likner på `member', men denne bruker `equal?' som test for likhet.

;; Konstant minnebruk:
(define (cycle? lst)
  (define (iter tortoise hare)
    (and (not (null? hare))
         (not (null? (cdr hare)))
         (or (eq? (cdr hare) tortoise)
             (iter (cdr tortoise) (cddr hare)))))
  (and (not (null? lst))
       (iter lst (cdr lst))))

;; Denne løsningen er basert på "Floyd's tortoise and hare algorithm":
;; http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare



;; 3 d
;;
;; Fra R5RS: "By definition, all lists have finite length and are
;; terminated by the empty list". 
;;
;; `bah' oppfyller ingen av kravene, i motsetning til `bar'.


;; 3 e+f

;; først tar vi med noen hjelpeprosedyrer:

(define (nth-pair lst n)
  (if (<= n 0)
      lst
      (nth-pair (cdr lst) (- n 1))))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;; Kommentarer: Her tenker vi på en ring som en enkel form for
;; sirkulær liste der cdr av det siste paret har blitt satt til å peke
;; på det første paret. (Lista "biter seg selv i halen".) Selve
;; ring-objektet er likevel representert som en prosedyre. Vi lagrer
;; også en variabel som peker til det "siste" paret i ringen (før
;; top), slik at både insert! og delete! kan utføres med konstant
;; kompleksitet, O(1), i likhet med left-rotate!. Derimot har
;; right-rotate! her lineær kompleksitet, O(n) (pga. kallet på
;; nth-pair). For at også denne operasjonen skal kunne gjøres i
;; konstant tid kan man f.eks. implementere ringen som en
;; dobbelt-lenket liste, noe som vil bety en god del mer kode.
;;
;; Merk at vi lager en kopi av input-lista så vi ikke kommer i skade
;; for å utilsiktet modifisere noe. Merk også at dette egentlig bare
;; er et utkast til en definisjon som ikke er helt ferdig: den mangler
;; feilsjekking og tar ikke høyde for at ringen kan være tom.


(define (make-ring elements)
  (let* ((top (append elements '())) ;; kopierer lista
	 (prev (last-pair top))
	 (size (length elements)))
    (set-cdr! prev top) ;; gjør lista sirkulær.
    (lambda (message . rest)
      (case message ;; illustrerer bruk av `case' i stedet for `cond'.
	((left-rotate!) (set! prev top) 
		        (set! top (cdr top)))
	((right-rotate!) (set! top prev)
		         (set! prev (nth-pair top (- size 1))))
	((insert!) (set! top (cons (car rest) top))
		   (set-cdr! prev top)
		   (set! size (+ size 1)))
	((delete!) (set! top (cdr top))
		   (set-cdr! prev top)
		   (set! size (- size 1))))
      (car top))))


;; Abstraksjonsbarriere for å gjøre grensesnittet mer generelt:

(define (top ring)
  (ring 'top))

(define (left-rotate! ring)
  (ring 'left-rotate!))

(define (right-rotate! ring)
  (ring 'right-rotate!))

(define (insert! ring element)
  (ring 'insert! element))

(define (delete! ring)
  (ring 'delete!))
