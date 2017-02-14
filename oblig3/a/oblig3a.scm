;; Oblig3a. Av Anders Jakob Sivesind og Torjus Dahle.
;;(require r5rs)
(load "prekode3a.scm")

;; (1 a og b)
;; Prosedyre som returnerer en memoisert versjon av input prosedyre.
(define (mem message proc)
  (define proc-table (make-table))
  (define (memo proc)
    (begin (define table (make-table))
           (insert! proc table proc-table)
           (lambda x
             (let ((computed-result (lookup x table)))
               (or computed-result
                   (let ((result (apply proc x)))
                     (insert! x result table)
                     result))))))
  (define (unmemo proc)
    (memo proc))
  (define (dispatch message proc)
    (cond ((eq? 'memoize message) (memo proc))
          ((eq? 'unmemoize message) (unmemo proc))
          ))
  (dispatch message proc))

;;(1c)
;; Feilen her ligger i at prosedyren ikke memoriserer hver kalkulering, men heller hvert kall på mem.

;;(1d)
;; Hjelpeprosedyre returnerer et table med keys og values fra input.
(define (help . args)
  (define table (make-table))
  (define (inner . args)
    (cond
      ((null? args) table)
      (else (begin (insert! (car args) (cadr args) table)
                   (apply inner (cddr args))))))
  (apply inner args))

;; Meningsløs prosedyre
(define (greet . args)
  (let* ((table (apply help args))
         (title (or (lookup 'title table) "friend"))
         (time (or (lookup 'time table) "day"))
         )
    (display (string-append "good " time " " title "\n"))))

;;(2a)
;; Gjør en liste til en stream
(define (list-to-stream list)
  (cond
    ((null? list) the-empty-stream)
    (else (cons-stream (car list) (list-to-stream (cdr list))))))

;; Gjør en stream til en liste
(define (stream-to-list stream . n)
  (cond
    ((stream-null? stream) '())
    ((null? n) (cons (stream-car stream) (stream-to-list (stream-cdr stream))))
    ((zero? (car n)) '())
    (else (cons (stream-car stream) (stream-to-list (stream-cdr stream) (- (car n) 1))))))

;;(2b)
;;Prosedyren tar et vilkårlig antall streams, og opererer på dem slik map gjør.
;;Prosedyren (og dermed strømmene) kuttes av hvis en av dem tar slutt.
(define (stream-map proc . argstreams)
  (if (one-stream-finished? argstreams)
      the-empty-stream 
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map 
                          (cons proc (map stream-cdr argstreams))))))
(define (one-stream-finished? streams) ;;Hjelpeprosedyre
    (cond 
      ((null? streams) #f)
      ((stream-null? (car streams)) #t)
      (else (one-stream-finished? (cdr streams)))))

;;(2c)
;;Problemet med remove-duplicates er det at den bruker memq, som igjen bruker car og cdr 
;;i stedet for stream variantene.

;;(2d)
;; Prosedyren fjerner alle symboler som er duplikater i en stream.
(define (remove-duplicates stream)
  (if (stream-null? stream)
    the-empty-stream
    (cons-stream (stream-car stream)
                  (stream-filter (lambda (x) (not (eq? x (stream-car stream))))
                                 (remove-duplicates (stream-cdr stream))))))

;;(2e)

;; (define x (stream-map show (stream-interval 0 10)))
;; Vil gi følgende output:
;; ~~~>0
;; På grunn av utsatt evaluering vil bare det første kallet på show utføres (med aktuelt parameter 0).
;; De andre "kallene" ligger bare der som promises som venter på å bli utført.

;; (stream-ref x 5)
;; Vil gi følgende output:
;; ~~~>1
;; ~~~>2
;; ~~~>3
;; ~~~>4
;; ~~~>5
;;->5
;;stream-ref forcer alle promisene fra 1 og til 5, slik at show vil bli utført for disse verdiene.

;; (stream-ref x 7)
;; Vil gi følgende output:
;; ~~~>6
;; ~~~>7
;; ->7
;; På samme måte som tidligere forces flere promises, de tidligere er allerede utført (0-5)
;, så her får vi bare output fra 6-7.

;;(2f)
;; Multipliserer to strømmer med hverandre
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;;Returnerer en strøm med fakulteter
(define factorials
  (cons-stream 1
               (mul-streams factorials nats)))