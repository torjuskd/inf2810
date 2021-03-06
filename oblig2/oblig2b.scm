;;Meldinger
(define (no-args)
  (display "Feil: Ingen argumenter oppgitt."))

;;(1a)
;;Prosedyre make-counter, returnerer en prosedyre 
;;som teller hvor mange ganger den har blitt kalt.

(define (make-counter)
  ((lambda (count) 
    (lambda () (set! count (+ count 1))
      count))
   0))

;;(1b) (se vedlagte filer)

;;(2a)

(define (make-stack items)
  (set! items (reverse items))
  (define (push . args)
    (cond
      ((null? args) (no-args))
      (else (set! items (append (reverse args) items)))))
  (define (pop)
    (cond
      ((null? items)'())
      (else (let ((first (car items)))
              (set! items (cdr items))
              first))))
  (define (stack) items)
  (define (dispatch message . args)
    (cond ((eq? 'push! message) (push args))
          ((eq? 'pop! message) pop)
          ((eq? 'stack message) stack)
          ))
  dispatch)


;;improved alah torjus    ;; huske ikke bruke " ' ", ved case
(define (make-stack stack)
  (lambda (message . rest)
    (case message
      ((push!)
       (for-each (lambda (x)
                   (set! stack (cons x stack)))
                 rest))
      ((pop)
       (if (not (null? stack))
           (let ((item (car stack)))
             (set! stack (cdr stack))
             item)))
      ((stack) stack)
      (else 'error))))
;;og enklere grensesnitt (oppgaven nedenfor):
(define (stack s)
  (s 'stack))

(define (pop! s)
  (s 'pop!))

(define (push! s . rest)
  (apply s (cons 'push! rest)))




;;(2b)
(define (pop! stack)
  ((stack 'pop!)))
;;(define (old-push! stack . args)
;;  (define (push-rest list)
;;    (if (null? list)
;;        (no-args)
;;        ((stack 'push!) (car list)
;;        (if (not (null? (cdr list)))
;;            (push-rest (cdr list))))))
;;  (push-rest args))
(define (push! stack . args)
  (cond
    ((null? args) '())
    ((list? args) (push! (car args)))
    (else (and ((stack 'push!) (car args))
               (push! (cdr args))))))
      
(define (stack stack)
  ((stack 'stack)))

;;(3a)
;;            +-+--+----+      +-+--+----+       +----+----+       +----+----+     +----+----+
;; bar +----->+    |    +----> |    |    +-----> |    |    +-----> |    |    +---> |    |  / |            (strukturen til bar etter kallet på define)
;;            +----+----+      +----+----+       +-+--+----+       +-+--+----+     +-+--+----+
;;              |                |                 |                 |               |
;;              v                v                 v                 v               v
;;            +---+            +---+             +---+             +---+           +---+
;;            | a |            | b |             | c |             | d |           | e |
;;            +---+            +---+             +---+             +---+           +---+



;;                                +---------------------------------------+
;;                                |                                       |
;;                                |                                       |
;;                                v                                       |
;;            +----+----+      +--+-+----+       +----+----+       +----+-+--+     +----+----+
;; bar +----->+    |    +----> |    |    +-----> |    |    +-----> |    |    |     |    |  / |             (strukturen til bar etter kallet etter set-cdr!)
;;            +-+--+----+      +-+--+----+       +-+--+----+       +-+--+----+     +-+--+----+
;;              |                |                 |                 |               |
;;              v                v                 v                 v               v
;;            +---+            +---+             +---+             +---+           +---+
;;            | a |            | b |             | c |             | d |           | e |
;;            +---+            +---+             +---+             +---+           +---+

  
;;(3c)
(define (cycle? items)
  (define (visited? elem visited)
    (cond 
      ((null? visited) #f)
      ((eq? elem (car visited)) #t)
      (else (visited? elem (cdr visited)))))
  (define (iter current visited)
    (cond
      ((null? current) #f)
      ((null? visited) (iter (cdr current)  (cons (car current) visited)))
      ((visited? (car current) visited) #t)
      (else (iter (cdr current) (cons (car current) visited)))))
  (iter items '()))

;;(3d)
;;Grunnen til at bah regnes som en liste, mens bar ikke gjør det, er at bah er null-terminert og bar ikke termineres.
      
;;(3e)
(define (make-ring items)
  (define ring-length (length items))
  (define topmost 0)
  (define (connect items2) ;;setter til å bite seg selv i halen
    (if (null? (cdr items2))
        (set-cdr! items2 items)
        (connect (cdr items2))))
  (connect items)
  (define (top)
    (list-ref items topmost))
  (define (left-rotate) 
    (set! topmost (+ 1 topmost))
    (top))
  (define (right-rotate)
    (set! topmost (- topmost 1))
    (if (= -1 topmost)
        (set! topmost (- ring-length 1)))
    (top))
  (define (delete)
    (define (inner items count)
    (if (< count (- topmost 1))
         (inner (cdr items) (+ 1 count))
         (and (and (set-cdr! items (cddr items))
          (set! ring-length (- ring-length 1)))
              (right-rotate))))
    (inner items 0))
  (define (insert item)
    (define (inner items count)
      (if (< count (- topmost 1))
          (inner (cdr items) (+ 1 count))
          (and (set-cdr! items (cons item (cdr items)))
                    (and (set! ring-length (+ 1 ring-length))
                         (top)))))
    (inner items 0))
  (define (dispatch message)
    (cond ((eq? 'left-rotate message) left-rotate)
          ((eq? 'right-rotate message) right-rotate)
          ((eq? 'delete message) delete)
          ((eq? 'insert message) insert)
          ((eq? 'top message) top)
          ))
  dispatch
  )

(define (top ring)
  ((ring 'top)))
(define (left-rotate! ring)
  ((ring 'left-rotate)))
(define (right-rotate! ring)
  ((ring 'right-rotate)))
(define (delete! ring)
  ((ring 'delete)))
(define (insert! ring value)
  ((ring 'insert)value))

;;(3f)
;; left-rotate! og right-rotate! har konstant kjøretid og minnebruk (O(1)).
;; (Dette er fordi vi bare inkrementerer/dekrementerer en variabel,
;; det er helt uavhengig av input-størrelse.)
;; Med insert! og delete! må vi potensielt traversere hele lista
;; fordi topmost kan være det "siste" elementet i ringen.
;; De får derfor lineær minnebruk og kjøretid (0(n) med avg. case 0(n/2)).


;;1.b) Torjus
;;3.a) Anders J.
;;3.b) Torjus
;;3.d) Anders J.
;;3.f) Torjus
