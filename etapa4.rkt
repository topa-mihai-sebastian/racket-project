#lang racket
(require racket/match)
(require "etapa2.rkt")
(provide (all-defined-out))

;; Această etapă continuă seria aplicațiilor heap-urilor  
;; de împerechere, pe care le vom folosi pentru a calcula
;; în mod dinamic mediana recenziilor unui film, simulând
;; condițiile din realitate - în care apar în permanență
;; noi recenzii pentru diverse filme.
;;    
;; Pentru a modela această dinamică folosim un flux de
;; perechi (nume-film . rating), pe baza căruia calculăm
;; un flux de stadii evolutive astfel:
;;  - fiecare stadiu este reprezentat ca listă de perechi
;;    * o pereche pentru fiecare film cu minim o recenzie
;;    * fiecare pereche este de forma
;;      (nume-film . mediană-rating-uri-primite-până-acum)
;;  - fiecare nouă recenzie determină actualizarea unei
;;    mediane, adică trecerea într-un alt stadiu,
;;    generând un nou element în fluxul rezultat
;;
;; Algoritmul folosit este următorul:
;;  Fluxul de perechi este transformat într-un flux de
;;  liste de cvartete (nume-film delta max-ph min-ph)
;;   - fiecare element din flux conține câte un cvartet 
;;     pentru fiecare film care are minim o recenzie
;;   - dacă filmul are un număr par de recenzii:
;;     - max-ph și min-ph au aceeași dimensiune
;;     - delta = size(max-ph) - size(min-ph) = 0
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este media rădăcinilor celor 2 PH-uri
;;   - dacă filmul are un număr impar de recenzii:
;;     - max-ph are un element în plus față de min-ph
;;     - delta = size(max-ph) - size(min-ph) = 1
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este rădăcina lui max-ph
;;
;; Pentru completarea cu succes a etapei este necesar să
;; calculați medianele cu algoritmul descris în enunț.
;; În caz contrar, punctajul acordat de checker va fi retras.


; TODO 1 (45p)
; add-rating : (Symbol, Int, PH, PH) x Number
;              -> (Symbol, Int, PH, PH)
; in: cvartet (nume delta max-ph min-ph),
;     rating de adăugat
; out: cvartet actualizat prin adăugarea 
;      rating-ului, astfel:
;  - dacă rating <= root(max-ph)
;    inserează rating în max-ph, actualizând delta
;  - altfel
;    inserează rating în min-ph, actualizând delta
;  - dacă delta > 1
;    mută root(max-ph) în min-ph
;  - dacă delta < 0
;    mută root(min-ph) în max-ph

(define (add-rating quad rating)
  (let* ((nume (first quad))
         (delta (second quad))
         (max-ph (third quad))
         (min-ph (fourth quad)))
    ;; daca rating <= se baga in max-ph
    (if (<= rating (ph-root max-ph))
        (let* ((new-max-ph (ph-insert (merge-f >) rating max-ph)))
          ;; daca max-ph devine prea mare mut radacina max-ph in min-ph
          (if (> (+ delta 1) 1)
              (let* ((new-min-ph (ph-insert (merge-f <) (ph-root new-max-ph) min-ph))
                     (updated-max-ph (ph-del-root (merge-f >) new-max-ph)))
                (list nume 0 updated-max-ph new-min-ph)) ;; reset : delta=0
              (list nume (+ delta 1) new-max-ph min-ph))) ;; else delta++
        ;; else adaug in min-ph
        (let* ((new-min-ph (ph-insert (merge-f <) rating min-ph)))
			(if (< (+ delta 1) 1);;daca min-ph devine prea mare
				;;mut radacina din min-ph in max-ph
				(let* ((new-max-ph (ph-insert (merge-f >) (ph-root new-min-ph) max-ph))
						(updated-min-ph (ph-del-root (merge-f <) new-min-ph)))
						;;reset delta =0
						(list nume 0 new-max-ph updated-min-ph))
			;;else delta --
			(list nume (- delta 1) max-ph new-min-ph))))))

; TODO 2 (45p)
; reviews->quads : Stream<(Symbol, Number)> ->
;                  Stream<[(Symbol, Int, PH, PH)]>
; in: stream de perechi (nume . rating)
; out: stream de liste de cvartete
;      (nume delta max-ph min-ph)
;  - elementul k din rezultat corespunde primelor
;    k recenzii din input (ex: dacă primele 10
;    recenzii sunt pentru 3 filme distincte, al
;    10-lea element din fluxul rezultat conține o
;    listă de 3 cvartete - unul pentru fiecare film)
; RESTRICȚII (20p):
;  - Lucrați cu operatorii pe fluxuri, fără a
;    converti liste în fluxuri sau fluxuri în liste.

(define (reviews->quads reviews)
  ;;reviews flux de intrare, se consuma
  ;; quads lista curenta
  (let loop ([reviews reviews] [quads '()])
    (if (stream-empty? reviews)
        empty-stream
		;;else
        (let* (;; iau primul el din flux
               [review (stream-first reviews)]
               [name (car review)]
               [rating (cdr review)]
               
               ;;daca filmul exista deja in quads
               [existing (assoc name quads)]
               
			   ;;daca filmul exista, acutalizam cvartetul cu noul rating
				;;else creez 
               [new-quad (if existing
                           (add-rating existing rating)
						   ;; delta=1(primul element)
                           (list name 1 (ph-insert (merge-f >) rating empty-ph) ;;max-ph
						    empty-ph))]  ; min-ph(initial gol)
               
			   ;;daca filmul exista inlocuim vechiul cvartet
			   ;;daca nu inlocuim noul cvartet
               [new-quads (cons new-quad 
                              (if existing (remove existing quads) 
                                  quads))])
          
		  ;;adaug noul set de cvartete la stream
		  ;; loop recursiv
          (stream-cons new-quads 
                      (loop (stream-rest reviews) new-quads))))))

; TODO 3 (30p)
; quads->medians : Stream<[(Symbol, Int, PH, PH)]> ->
;                  Stream<[(Symbol, Number)]>  
; in: stream de liste de cvartete (ca mai sus)
; out: stream de liste de perechi (nume-film . mediană)
;  - mediana se calculează pe baza PH-urilor din
;    fiecare cvartet, conform algoritmului de mai sus
; RESTRICȚII (20p):
;  - Nu folosiți recursivitate explicită. Folosiți cel
;    puțin o funcțională pe fluxuri.

(define (quads->medians quads)
  ;; verific daca streamul de input e gol
  (if (stream-empty? quads)
      quads
      ;; else procesam fiecare el
      (stream-map
       (lambda (quads)
         ;; trans cvartetele separate in perechi (nume . mediană)
         (map (lambda (quad)
                (let* ([name (first quad)]
                       [delta (second quad)]    ; dif de dimens
                       [max-ph (third quad)]    ; elemente mai mici
                       [min-ph (fourth quad)])  ; elemente mari
                  (cons name
                        (if (zero? delta)
                            ;; caz par
                            (/ (+ (ph-root max-ph) (ph-root min-ph)) 2)
                            ;; caz impar
                            (ph-root max-ph)))))
              quads))  ;; pentru fiecare quad
       quads)))  ;; tot fluxul
