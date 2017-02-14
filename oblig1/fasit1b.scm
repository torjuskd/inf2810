;;;;
;;;; Løsningsforslag til innlevering (1b), INF2810, våren 2016
;;;; (erikve og oe)
;;;; 


;;;
;;; Oppgave 1 (3 poeng)
;;;

;; 1 a)
;; 
;; (cons 47 11) --> (47 . 11)
;;
;;
;;   +-------+-------+
;;   |       |       |
;;   |   o   |   o---+------>  11
;;   |   |   |       |
;;   +---+---+-------+
;;       |
;;       |
;;       |
;;       V
;;
;;       47


;; 1 b)
;; 
;; (cons 47 '()) --> (47)
;;
;;
;;   +-------+-------+
;;   |       |    /  |
;;   |   o   |   /   |
;;   |   |   |  /    |
;;   +---+---+-------+
;;       |
;;       |
;;       |
;;       V
;;
;;       47


;; 1 c) 
;;
;; (list 47 11) --> (47 11)
;;
;;
;;   +-------+-------+        +-------+-------+ 
;;   |       |       |        |       |    /  |
;;   |   o   |   o---+------> |   o   |   /   |
;;   |   |   |       |        |   |   |  /    |
;;   +---+---+-------+        +---+---+-------+ 
;;       |                        |
;;       |                        |
;;       |                        |
;;       V                        V 
;;
;;       47                       11    


;; 1 d)
;;
;; '(47 (11 12)) --> (47 (11 12))
;;
;;
;;   +-------+-------+        +-------+-------+ 
;;   |       |       |        |       |    /  | 
;;   |   o   |   o---+------> |   o   |   /   |  
;;   |   |   |       |        |   |   |  /    | 
;;   +---+---+-------+        +---+---+-------+ 
;;       |                        |
;;       |                        |
;;       |                        |
;;       V                        V
;; 
;;       47                   +-------+-------+        +-------+-------+
;;                            |       |       |        |       |    /  |
;;                            |   o   |   o---+------> |   o   |   /   |
;;                            |   |   |       |        |   |   |  /    |
;;                            +---+---+-------+        +---+---+-------+
;;                                |                        |
;;                                |                        |
;;                                |                        |
;;                                V                        V
;;
;;                                11                       12
			    
			    
;; 1 e)			    
;;			    
;; (define foo '(1 2 3))    
;; (cons foo foo) --> ((1 2 3) 1 2 3)
;;			    
;;			    
;;         +-------+-------+
;;         |       |       |
;;         |   o   |   o   |
;;         |   |   |  /	   |
;;         +---+---+-/-+---+
;;             |    /  	    
;;             |   /   	    
;;             |  /    	    
;;             V L     	    
;;		 	    
;;         +-------+-------+        +-------+-------+        +-------+-------+
;;         |       |       |        |       |       |        |       |    /  |
;; foo---> |   o   |   o---+------> |   o   |   o---+------> |   o   |   /   |
;;         |   |   |       |        |   |   |       |        |   |   |  /    |
;;         +---+---+-------+        +---+---+-------+        +---+---+-------+
;;             |                        |                        |
;;             |                        |                        |
;;             |                        |                        |
;;             V                        V                        V
;;			    
;;             2                        2                        3
			    
			    
;; 1 f)			    
;;			    
;; (car (cdr (cdr '(1 2 3 4))))
;; 			    
;; nb: det finnes også en "forkortelse" for dette: 
;;			    
;; (caddr '(1 2 3 4)) 	    
			    
			    
;; 1 g)			    
;;
;; (car (car (cdr '((1 2) (3 4))))) 
;;
;; eller
;;
;; (caadr '((1 2) (3 4))) 


;; 1 h)
;;
;; (car (car (cdr (cdr '((1) (2) (3) (4)))))) 
;;
;; eller
;;
;; (caaddr '((1) (2) (3) (4))) 


;; 1 i)
;;
;; (cons (cons 1 (cons 2 '())) 
;;       (cons (cons 3 (cons 4 '())) '()))
;;
;;
;; (list (list 1 2) 
;;       (list 3 4))


;;;
;;; Oppgave 2 (1/2 + 1/2 + 1 + 1/2 + 1 + 1 + 1 + 1 + 1/2 Poeng)
;;;

;; 2 a)

(define (length2 items)
  (define (iter rest n)
    (if (null? rest) 
	n
        (iter (cdr rest) (+ n 1))))    
  (iter items 0))


;; 2 b)

(define (rev-list list)
  (define (rev-it in out)
    (if (null? in)
        out
        (rev-it (cdr in) (cons (car in) out)))) 
  (rev-it list '()))

;;
;; Her har vi valgt en halerekursiv variant: Rekursjonen skjer i den
;; interne hjelpeprosedyren 'rev-it', og det rekursive kallet står i
;; haleposisjon; vi ser at det ikke er noe mer som gjenstår å gjøre i
;; prosedyrekroppen etter det rekursive kallet.  Dette resulterer i en
;; iterativ prosess med en minnebruk som er konstant og en tidsbruk
;; som er lineær i lengden på input lista.  Grunnen til at vi har
;; valgt en halerekursiv struktur er egentlig ikke på grunn av
;; effektivitetshensyn men fordi det er den enkleste måten å bygge opp
;; listen i motstatt rekkefølge: Vi begynner med å cons'e det første
;; elementet på den tomme lista, og deretter cons'er vi på et nytt
;; element og bygger opp liste bakvendt i forhold til hvordan vi leser
;; den inn.  Hvis vi derimot hadde en brukt en struktur ala
;;
;; (cons (car list) (rev (cdr list))) 
;;
;; så ville vi i stedet fått ut lista i samme rekkefølge som originalen. 
;;


;; 2 c)

(define (ditch i items)
  (cond ((null? items) '())
	((= i (car items))
	 (ditch i (cdr items)))
	(else (cons (car items)
		    (ditch i (cdr items))))))

;;
;; Denne prosedyren vil gi opphav til en rekursiv prosess med lineær
;; vekst i både tid og minne som er proporsjonal med antall elementer
;; i input-lista.  Vi tar her utgangspunkt i et "worst case"-scenario
;; der lista ikke inneholder noen elementer som skal forkastes og
;; hvert rekursive kall etterlater et ventende kall på 'cons'.  Men vi
;; ser at hver gang vi faktisk finner et element som skal forkastes
;; vil rekursjonen skje i haleposisjon.  I det best tenkelige tilfellet
;; (med tanke på kompleksitet) der alle elementer i input-lista ville
;; blitt forkastet (alle matcher 'i') så ville vi dermed i stedet fått
;; en iterativ prosess med konstant minnebruk.
;;
;; Når det gjelder å bygge opp den nye resultatslista så er det mest
;; naturlig å gjøre det ved å "cons'e" elementer på de rekursive
;; kallene på 'ditch'.  Som vi så i oppgaven over så ville jo en enkel
;; halerekursiv variant her heller ha bygget opp lista baklengs, og
;; dermed måtte vi i så fall til slutt ha brukt tid på reverse den nye
;; lista før denne kunne returneres.
;;

;; 2 d)

(define (nth n items)
  (if (zero? n)
      (car items)
      (nth (- n 1) (cdr items))))


;; 2 e)

(define (where x seq)
  (define (w-iter rest pos)
    (cond ((null? rest) #f)
	  ((= x (car rest)) pos)
	  (else (w-iter (cdr rest) (+ pos 1)))))
  (w-iter seq 0))


;; 2 f)

(define (map2 proc list1 list2)
  (if (or (null? list1) 
	  (null? list2))
      '()
      (cons (proc (car list1)
                  (car list2))
            (map2 proc 
		  (cdr list1) 
		  (cdr list2)))))


;; 2 g)

;; gjennomsnitt:
;;
;; (map2 (lambda (x y) 
;;          (/ (+ x y) 2)) 
;;       '(1 2 3 4) '(3 4 5))
;;
;; --> (2 3 4)

;; sjekk for partall:
;;
;; (map2 (lambda (x y) 
;;          (and (even? x) 
;;               (even? y))) 
;;       '(1 2 3 4) '(3 4 5))
;;
;; --> (#f #t #f)


;; 2 h)

(define (both? pred)
  (lambda (x y)
    (and (pred x)
	 (pred y))))


;; 2 i)

(define (self proc)
  (lambda (x)
    (proc x x)))
