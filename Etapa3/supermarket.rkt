#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue)
  )

(define (update f counters index)
  (cond
    [(empty? counters) '()] 
    [(equal? index (counter-index (car counters)))  (cons (f (car counters)) (update f (cdr counters) index))]
    [(cons (car counters ) (update f (cdr counters) index))]
   )
 )

(define tt+
   (lambda(minutes)
    (lambda(C)
     (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C))
      )))

(define (et+ minutes)
    (lambda(C)
      (make-counter (counter-index C)  (+ (counter-tt C) minutes)  (+ (counter-et C) minutes) (counter-queue C))
  ))
  

(define (add-to-counter name n-items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
  (match C
    [(counter index tt et queue)
 (if (queue-empty? queue) 
              (make-counter index (+ tt n-items)  n-items (make-queue (queue-left queue)(cons (cons name n-items) (queue-right queue)) (queue-size-l queue) (+ (queue-size-r queue) 1)))
              (make-counter index (+ tt n-items) et (make-queue (queue-left queue)(cons (cons name n-items) (queue-right queue)) (queue-size-l queue) (+ (queue-size-r queue) 1))))
  ])))


(define (minimum acc lst f ) 
  (cond
    [(null? lst) acc]
    [(> (f acc) (f (car lst))) (minimum (car lst) (cdr lst) f )]
    [(equal? (f acc) (f (car lst)))
     (if (> (counter-index acc) (counter-index (car lst)))
         (minimum (car lst) (cdr lst) f) (minimum acc (cdr lst) f))]
          [(< (f acc) (f (car lst))) (minimum acc (cdr lst) f )]
   ))

 

(define (min-tt counters)
 (cons (counter-index (minimum (car counters) (cdr counters)  counter-tt )) (counter-tt (minimum (car counters) (cdr counters) counter-tt)))
  )

(define (min-et counters)
  
  (cons (counter-index (minimum (car counters) (cdr counters) counter-et)) (counter-et (minimum (car counters) (cdr counters) counter-et)))
  )

(define (remove-first-from-counter C)   ; testată de checker
   (match C
    [(counter index tt et queue)
  (if (queue-empty? (dequeue queue))
      (make-counter index (- tt et) 0  (dequeue queue))
      (make-counter index (- tt et) (cdr (car (queue-left (dequeue queue))))  (dequeue queue)))]))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
    [(counter index tt et queue)
     (if (equal? et 0) C
    (if (positive? (- tt minutes ))
                   (if (positive? (- et minutes))
                       (make-counter index (- tt minutes) (- et minutes) queue)
                       (make-counter index (- tt minutes) 0 queue)
                       )
                   (if (positive? (- et minutes))
                       (make-counter index 0 (- et minutes) queue)
                       (make-counter index 0 0 queue)
                       )
     ))])))

(define (update_counter C minutes)
  (match C
    [(counter index tt et queue)
     (if (zero? minutes) C
    (if (positive? (sub1 et))
       (update_counter (make-counter index (sub1 tt) (sub1 et) queue) (sub1 minutes))
       (update_counter (remove-first-from-counter C) (sub1 minutes))
       )
   )
  ])

  )
     
(define (pass-time-through-all-counters counters minutes)
  (if (empty? counters)
      '()
        (if (zero? (counter-et (car counters)))
          (cons (car counters) (pass-time-through-all-counters (cdr counters) minutes) )
          (cons (update_counter (car counters) minutes) (pass-time-through-all-counters (cdr counters) minutes))
          )
       )
          
      )
   
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

(define (ttmed counters med)
  (if (empty? counters)
      med
      (ttmed (cdr counters) (+ med (counter-tt (car counters))))
    )
 )

(define (update_queue counters minutes queue)
  (if (empty? counters)
      queue
      (if (zero? (counter-et (car counters)))
          (update_queue (cdr counters) minutes queue)
          (cons (upd (car counters) minutes null) (update_queue (cdr counters) minutes queue)))
          
      )
   )

(define (upd C minutes queue-list)
  (match C
    [(counter index tt et queue)
     (if (zero? minutes) queue-list
    (if (positive? (sub1 et))
       (upd (make-counter index (sub1 tt) (sub1 et) queue) (sub1 minutes) queue-list)
       (if (queue-empty? (counter-queue C)) (upd (remove-first-from-counter C) (sub1 minutes) queue-list)
       (upd (remove-first-from-counter C) (sub1 minutes)  (append (list ( cons (counter-index C)  (car(top(counter-queue C))))) queue-list))
       )
       )
   )
  ])
  )

(define (help_to_serve requests fast-counters slow-counters queue-list)
   (if (null? requests)
      (append queue-list (append fast-counters slow-counters))
      
      (match (car requests)
       [(list 'delay index minutes)
        (help_to_serve (cdr requests) (update (et+ minutes) fast-counters index)
                  (update (et+ minutes) slow-counters index) queue-list)
         ]
        
        [(list name nr)
         (if (equal? name 'ensure)
             (if (> ( / (ttmed (append fast-counters slow-counters ) 0)  (length (append fast-counters slow-counters))) nr)
                 (help_to_serve requests fast-counters (append slow-counters  (list(empty-counter (+ 1(length (append fast-counters slow-counters)))))) queue-list)
                 (help_to_serve (cdr requests) fast-counters slow-counters queue-list)
              )
             (if (< ITEMS  nr)
                 (help_to_serve (cdr requests) fast-counters (update (add-to-counter name nr) slow-counters (car (min-tt slow-counters))) queue-list)
                 (help_to_serve (cdr requests) (update (add-to-counter name nr) fast-counters (car (min-tt (append slow-counters fast-counters))))
                        (update (add-to-counter name nr) slow-counters (car (min-tt (append slow-counters fast-counters)))) queue-list))
           )
         ]

        [ minutes
         (help_to_serve (cdr requests) (pass-time-through-all-counters  fast-counters minutes) (pass-time-through-all-counters slow-counters minutes)
                        (update_queue (append fast-counters slow-counters) minutes queue-list) )
        ]
        )
  )
  )
  

(define (serve requests fast-counters slow-counters)
  (help_to_serve requests fast-counters slow-counters '()) 
)

        
