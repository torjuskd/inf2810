;;;;
;;;; Løsningsforslag til innlevering (2a), INF2810, våren 2016
;;;; (erikve og oe)
;;;; 

;;;
;;; Forutsetter at man allerede har lastet prekoden i ‘huffman.scm’, f.eks. ved:
;;;
(load "huffman.scm")


;;;
;;; Oppgave 1 (2.5 poeng)
;;;

;;
;; 1a 
;;
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (p q) p)))

(define (p-cdr proc)
  (proc (lambda (p q) q)))

;;
;; 1b
;;
;; ((lambda (x y)
;;    (+ foo x y))
;;  foo 20)
;;
;;
;; ((lambda (foo)
;;    ((lambda (x y)
;;       (+ foo x y))
;;     foo 20))
;;  10)
;;
;;
;; Det første uttrykket returnerer 80 mens det andre returnerer
;; 40.  Når `x' bindes til `foo' i det første uttrykket er det den
;; globale variabelen `foo' det er snakk om (som har verdien 30), og
;; det samme gjelder referansen til `foo' i kroppen av uttrykket.  I
;; det andre uttrykket derimot bindes `x' til verdien av en lokal
;; variabel `foo': Denne er bundet til 10 og "overskygger" den
;; eksisterende globale bindingen til `foo'.  Det samme gjelder
;; referansen til `foo' i kroppen av uttrykket.
;;


;;
;; 1c
;;
;; Uttrykket evaluerer til: (6 -4 21 1/2)
;;
;; Førsteargumentet til `map' er en anonym prosedyre som forventer tre
;; argumenter, `x', `y', og `z', der det midterste argumentet `y'
;; forventes å være en prosedyre som så kalles med `x' og `z' som
;; argumenter.  Av listeargumentene til `map' består det andre av
;; prosedyrer som vil bli anvendt på par av elementer fra samme
;; posisjon i den første og siste lista.  Vi kan også kalle
;; lambda-uttrykket direkte på et sett av argumenter, f.eks. som
;;
;; ((lambda (x y z) (y x z)) 1 + 2)  -->  3
;;
;; ((lambda (x y z) (y x z)) 2 expt 11) --> 2048
;;
;; Én måte å tenke på hva lambda-uttrykket gjør er at det lar oss
;; evaluere binære funksjonsuttrykk skrevet på "infix-form".
;;


;;;
;;; Oppgave 2 (7.5 poeng)
;;;

;;
;; 2a
;;
(define (member? symbol list)
  (cond ((null? list) #f)
        ((eq? symbol (car list)) #t)
        (else (member? symbol (cdr list)))))
;;
;; (member? 'forest '(lake river ocean)) --> #f
;; (member? 'river '(lake river ocean)) --> #t
;;

;;
;; 2b
;;
;; Den internt definerte prosedyren `decode-1' tar to formelle
;; parametre; `bits' og `current-branch'.  `decode-1' bruker verdien
;; til `tree' bare som initialisering av `current-branch' som så
;; oppdateres gjennom rekursjonen.  Men i tillegg bruker `decode-1'
;; også variabelen `tree': Selv om denne ikke er et formelt parameter
;; så spenner rekkevidden til dens binding over definisjonen av
;; `decode-1' slik at den likevel er tilgjengelig.  Parameteret
;; `current-branch' re-initialiseres til verdien av `tree' hver gang
;; vi når en løvnode og dekoding begynner fra toppen av treet igjen.
;; Hvis vi skulle oppnådd det samme med `decode' direkte måtte den ha
;; tatt tre parametre, og bli kalt ved f.eks `(decode bits tree tree)'.

;;
;; 2c  
;;
(define (decode bits tree)
  (define (decode-it bits current-branch message)
    (if (null? bits)
      (reverse message)
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)       
          (decode-it (cdr bits) tree (cons (symbol-leaf next-branch) message))
          (decode-it (cdr bits) next-branch message)))))
  (decode-it bits tree '()))

;;
;; 2d
;;
;; (decode sample-code sample-tree) --> (ninjas fight ninjas by night)
;;

;;
;; 2e
;;
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
    '()
    (let* ((left (symbols (left-branch tree)))
           (bit (if (member? symbol left) 0 1)))
      (cons bit (encode-symbol symbol (choose-branch bit tree))))))

;;
;; 2f
;;
(define (grow-huffman-tree pairs)
  (define (grow nodes) 
    (if (null? (cdr nodes))
      (car nodes) ;; root
      (let* ((left (car nodes))
             (right (cadr nodes))
             (new (make-code-tree left right)))
        (grow (adjoin-set new (cddr nodes))))))
  (grow (make-leaf-set pairs)))

;;
;; 2g
;;
;; Først lager vi noen lister som holder alfabetet vårt og meldingen.
;; Så genrerer vi et Huffman-tre for alfabetet, før vi koder meldingen:
;;

(define alphabet
  '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3)
    (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by 12)
    (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))

(define message 
  '(ninjas fight 
    ninjas fight ninjas 
    ninjas fight samurais 
    samurais fight
    samurais fight ninjas 
    ninjas fight by night))

(define codebook (grow-huffman-tree alphabet))

;;
;; Vi kan nå kode meldingen og finne lengden:
;;
;; (length (encode message codebook))  -->  43
;;
;; Vi trenger altå 43 bits.  Det er 17 ord i meldingen:
;;
;; (length message) --> 17 
;;
;; (exact->inexact (/ (length (encode message codebook)) (length message)))
;;
;; Antall bits i snitt er altså 43 / 17 = 2.53
;; 
;; Hvis vi i stedet skulle brukt en fastlengdekode ville vi trengt 68
;; bits.  Forklaring: Generelt er det slik at for et alfabet med n
;; symboler vil vi trenge log2(n) bits hvis vi vil bruke en kode med
;; fast lengde.  Det er 16 symboler i alfabetet vi har blitt gitt og vi
;; ville dermed trengt log2(16) = 4 bits per symbol.  Siden det er 17
;; symboler i meldingen vi skal kode måtte vi altså brukt 17 * 4 = 68
;; bits.
;;

;;
;; 2h
;;
(define (huffman-leaves tree)
  (if (leaf? tree)
    (list (list (symbol-leaf tree) (weight-leaf tree)))
    (append (huffman-leaves (left-branch tree)) 
            (huffman-leaves (right-branch tree)))))

;;
;; 2i
;;
;; Denne kan skrives mer kompakt men på bekostning av lesbarheten.  Her
;; illustrerer vi også bruk av `let' og høyereordens prosedyrer.
;;
(define (expected-code-length tree)
  (let* ((leaves (huffman-leaves tree))
         (symbols (map car leaves))
         (freqs (map cadr leaves))
         (sum (apply + freqs))
         (lengths (map (lambda (s) 
			 (length (encode-symbol s tree))) 
		       symbols))
         (probs (map (lambda (f) (/ f sum)) freqs))
         (products (map * probs lengths)))
    (apply + products)))

;;
;; (expected-code-length sample-tree) --> 8/5
;;

;;
;; En student leverte en annen fin og kompakt løsning i fjor:
;;
(define (expected-code-length tree)
  (define (subtree-sum subtree depth)
    (if (leaf? subtree)
      (* (weight subtree) depth) ;; frekvens * kodelengde
      (+ (subtree-sum (left-branch subtree) (+ depth 1))
         (subtree-sum (right-branch subtree) (+ depth 1)))))
  (/ (subtree-sum tree 0) (weight tree)))

;;
;; For at faktisk kodelengde ikke skal overskride den forventede
;; gjennomsnittslengden må de relative frekvensene til symbolene i
;; meldingen tilsvare de relative frekvensene til symbolene i
;; treet (dersom treet genereres på bakgrunn av selve meldingen som
;; faktisk skal kodes er dette naturligvis garantert).  Alternativt må
;; uventet høy forekomst av et symbol med langt kodeord veies opp av
;; tilsvarende uventet høy forekomst av symboler med korte kodeord.
;;
