#lang racket
(require "etapa2.rkt")
(require racket/match)
(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor heap-urilor de 
;; împerechere, pe care le vom folosi pentru:
;;  - a extrage cele mai bune filme dintr-o listă, conform
;;    cu un anumit criteriu
;;  - a extrage cele mai bune recenzii dintr-o colecție
;;    de recenzii pentru diverse filme (o recenzie bună
;;    corespunde unei note bune acordate filmului respectiv)
;;
;; Pentru completarea cu succes a etapei este necesar să
;; rezolvați sarcinile cu algoritmii dedicați PH-urilor 
;; (descriși în enunț). Punctajul acordat de checker va fi 
;; retras dacă sarcinile sunt rezolvate cu alți algoritmi.


; TODO 1 (40p)
; Definiți funcția best-k într-o formă care
; facilitează derivarea ulterioară a funcțiilor
; best-k-rating și best-k-duration.
; in: criteriu de comparație op (care compară 2 filme),
;     listă de filme movies, număr k
; out: lista sortată a celor mai "bune" filme
;      conform criteriului (cel mai "bun" primul)
; Algoritm:
;  1. construiește un PH de filme pe baza listei movies 
;     și a criteriului op
;  2. extrage în mod repetat root-ul acestui PH până
;     când rezultatul conține k filme (sau PH-ul devine vid)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului.

(define (movies->ph op movies)
  (foldl (lambda (movie ph)
           (ph-insert (merge-f op) movie ph))
         empty-ph
         movies))

(define (best-k op movies k)
  ;; Construim pairing heap-ul folosind movies->ph
  (let ((ph (movies->ph op movies))
        (merge (merge-f op))) ;; Generăm funcția de merge pe baza comparatorului op
    ;; Folosim named let pentru a extrage rădăcinile
    (let loop ((ph ph) (result '()) (count 0))
      (cond
        ;; Dacă am extras deja k filme sau PH-ul este vid, returnăm rezultatul
        [(or (>= count k) (null? ph)) (reverse result)]
        ;; Altfel, extragem rădăcina și continuăm
        [else
         (let ((root (ph-root ph))       ;; Extragem rădăcina
               (new-ph (ph-del-root merge ph))) ;; Ștergem rădăcina din PH folosind merge
           (loop new-ph (cons root result) (add1 count)))]))))


; best-k-rating : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai bune k filme din movies (ca rating)
; RESTRICȚII (5p):
;  - Obțineți best-k-rating ca aplicație a lui best-k.
(define best-k-rating
  (lambda (movies k)
  	(best-k (lambda (a b) (> (movie-rating a) (movie-rating b)))
		movies k)))

; best-k-duration : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai scurte k filme din movies 
; RESTRICȚII (5p):
;  - Obțineți best-k-duration ca aplicație a lui best-k.
(define m10  (make-movie 'hundreds-of-beavers  10 'comedy    '(1 48) '(slapstick action)))
(define m9   (make-movie '12-angry-men         9  'drama     '(1 36) '(legal)))
(define m8   (make-movie 'manchurian-candidate 8  'thriller  '(2 06) '(spy tragedy)))
(define m7   (make-movie 'm*a*s*h              7  'comedy    '(1 56) '(satire drama)))
(define m6   (make-movie 'pulse-kairo          6  'horror    '(1 59) '(supernatural mystery)))
(define m5   (make-movie 'trap                 5  'horror    '(1 45) '(crime thriller)))
(define ms10 (make-movie 'rear-window          10 'thriller  '(1 52) '(seen drama suspense)))
(define ms9  (make-movie 'm                    9  'thriller  '(1 39) '(seen crime mystery)))
(define ms8  (make-movie 'menilmontant         8  'drama     '(0 38) '(seen tragedy short)))
(define ms7  (make-movie 'neo-tokyo            7  'animation '(0 50) '(seen scifi fantasy)))
(define ms3  (make-movie 'maniac               3  'horror    '(0 51) '(seen b-horror)))


(define best-k-duration
  (lambda (movies k)
    (best-k (lambda (a b);; apare ora+minute deci trb sa fac conversie
              (< (+ (* 60 (first (movie-duration a))) (second (movie-duration a)))
                 (+ (* 60 (first (movie-duration b))) (second (movie-duration b)))))
            movies k)))
;(best-k-duration (list m8 ms9 ms3 m7) 4)

; TODO 2 (30p)
; update-pairs : ((Symbol, PH) -> Bool) x [(Symbol, PH)]
;                -> [(Symbol, PH)]
; in: predicat p, listă de perechi (nume-film . PH)
;     (PH este un max-PH care conține notele acordate
;      filmului în diverse recenzii - așadar un PH
;      de numere)
; out: lista pairs actualizată astfel:
;      - pentru prima pereche care satisface predicatul 
;        p, PH-ului perechii i se șterge rădăcina
;      - dacă PH-ul perechii este vid sau dacă nicio pereche
;        nu satisface p, se întoarce lista pairs nemodificată
; RESTRICȚII (20p):
;  - Folosiți named let pentru a itera prin perechi.
(define (update-pairs p pairs)
  'your-code-here)
                  

; TODO 3 (50p)
; best-k-ratings-overall : [(Symbol, PH)] x Int
;                          -> [(Symbol, Number)]
; in: listă de perechi (nume-film . PH)
;     (ca mai sus, PH este un max-PH de rating-uri)
;     număr k 
; out: lista sortată a celor mai bune k perechi
;      (nume-film . rating), corespunzând celor mai
;      bune rating-uri din toate PH-urile
; Algoritm:
;  1. Se inițializează un PH de perechi (nume . rating), 
;     corespunzând celui mai bun rating al fiecărui film
;     (adică se extrage rădăcina fiecărui PH de ratinguri,
;      în pereche cu numele filmului aferent)
;  2. Repetă de k ori:
;     - extrage rădăcina PH-ului de rădăcini
;       (name-root . rating-root)
;       (adică extrage cea mai bună pereche per total)
;     - adu în PH-ul de rădăcini următorul cel mai bun 
;       rating al filmului name-root (dacă există)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului.
(define (best-k-ratings-overall pairs k)
  'your-code-here)

