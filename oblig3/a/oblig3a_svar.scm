;; Løsningsforslag til oppgavesett 3a, INF2810, våren 2016.
;; erikve på ifi.uio.no



;;; 1 a + b

(define mem
  (let ((f-table (make-table)))
    ;; tabell for å huske de originale 
    ;; umemoiserte prosedyrene.
    (lambda (action f)
      (case action 
        ((unmemoize) (lookup f f-table))
        ((memoize) 
         (let* ((arg-table (make-table))
		;; tabell for å huske returverdiene
		;; for memoisert prosedyre.
                (mf (lambda args
                      (or (lookup args arg-table)
                          (let ((result (apply f args)))
                            (insert! args result arg-table)
                            result)))))
	   ;; ta vare på den opprinnelige prosedyren før 
           ;; vi returnerer den nye memoiserte versjonen:
           (insert! mf f f-table)
           mf))))))


;;; 1 c

;; Når vi i 1a brukte mem på følgende måte: 
;;
;; (set! fib (mem 'memoize fib))
;;
;; så overskrev vi definisjonen av fib. De rekursive kallene i den
;; opprinnelige definisjonen av fib vil da bruke den nye memoiserte
;; versjonen. Når vi i stedet navngir den nye meoiserte versjonen som
;; mem-fib ved
;;
;; (define mem-fib (mem 'memoize fib))
;;
;; så oppnår vi ikke lenger denne overskyggingen. De rekursive kallene
;; i fib kaller jo ikke memo-fib men den vanlige umemoiserte
;; versjonen av fib, og vi går dermed glipp av mye av effekten.



;;; 1 d

;; vi venter at args er en liste av navngitte argumenter på formen;
;; (navn1 verdi1 navn2 verdi1 ... navn-n verdi-n)

(define (get-arg key args default)
  (let ((a (member key args)))
    (if a (cadr a) default)))

(define (greet . args)
  (let ((time (get-arg 'time args "day"))
        (title (get-arg 'title args "friend")))
    (display (string-append "good " time " " title))))



;;; 2 a

(define (stream-to-list stream . args)
  (define (aux str n)
    (if (or (stream-null? str) (zero? n)) 
        '()
        (cons (stream-car str)
              (aux (stream-cdr str) (- n 1)))))
  (aux stream (if (null? args) -1 (car args))))


(define (list-to-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst)
                   (list-to-stream (cdr lst)))))


;;; 2 b

(define (stream-map proc . argstreams)
  (if (any? stream-null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map 
	      (cons proc (map stream-cdr argstreams))))))


;; hjelpeprosedyre for basistilfellet i stream-map:
(define (any? pred list)
  (cond ((null? list) #f)
	((pred (car list)) #t)    ;; stopp så fort vi får #t
	(else (any? pred (cdr list)))))


;;; 2 c

;;; `memq' terminerer ikke før den enten finner en duplikat eller har
;;; traversert hele sekvensen.  Siden strømmer kan være uendelige vil
;;; denne strategien kunne bety at prosedyren ikke terminerer.


;;; 2 d

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream) 
		   (stream-filter 
		    (lambda (x) (not (eq? (stream-car stream) x)))
		    (remove-duplicates (stream-cdr stream))))))


;; Vi tar også med en annen løsning som vil være mer effektiv men bryter
;; med mønseteret vi la opp til i oppgaveteksten:
;;
;; (define (remove-duplicates stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (cons-stream (stream-car stream)
;;                    (remove-duplicates
;;                     (stream-filter
;;                      (lambda (x) (not (eq? (stream-car stream) x)))
;;                      (stream-cdr stream))))))




;;; 2 f

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; Denne versjonen håndterer et vilkårlig antall strømmer:
;;
;;(define (mul-streams . streams)
;;  (apply stream-map * streams))

(define factorials 
  (cons-stream 1 (mul-streams nats factorials)))



