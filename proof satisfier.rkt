#lang racket

;; SAT / CNF SOLVER ;;

#|
A ~B ~C | '((0 3 5)
~A      |   (1)
B ~C    |   (2 5))
|#

(define (process-clauses-master)
  (define input-list (get-input-helper empty))
  (define map (get-map-master input-list))
  (define real-input (reverse (process-clauses input-list empty map)))
  (define elements (build-lst-master (get-elements real-input)))
  (dfs real-input elements (generate-sat-map elements)))

(define (get-elements input-list)
  (define 1dlist (to-1d input-list))
  (define (helper lst acc)
    (cond
      [(empty? lst) acc]
      [(> (first lst) acc) (helper (rest lst) (first lst))]
      [else (helper (rest lst) acc)]))
  (quotient (helper 1dlist 0) 2))

(define (build-lst-master num)
  (reverse (build-lst num)))

(define (build-lst num)
  (cond
    [(< num 0) empty]
    [else (cons num (build-lst (- num 1)))]))

(define (dfs lst elements sat-map)
  (cond
    [(empty? elements) (list #t sat-map)]
    [(satisfiable? lst (update sat-map (first elements) #t))
     (cond
       [(list? (dfs lst (rest elements) (update sat-map (first elements) #t)))
        (list #t (second (dfs lst (rest elements) (update sat-map (first elements) #t))))]
       [(satisfiable? lst (update sat-map (first elements) #f))
        (dfs lst (rest elements) (update sat-map (first elements) #f))]
       [else #f])]
    [(satisfiable? lst (update sat-map (first elements) #f))
     (dfs lst (rest elements) (update sat-map (first elements) #f))]
    [else #f]))

(define (satisfiable? lst sat-map)
  (define new-lst (match-map-to-lst lst sat-map))
  (define (satisfiable-helper new-lst)
    (cond
      [(empty? new-lst) #t]
      [(false? (get-truth (first new-lst))) #f]
      [else (satisfiable-helper (rest new-lst))]))
  (satisfiable-helper new-lst))

(define (match-map-to-lst lst sat-map)
  (cond
    [(empty? lst) empty]
    [else (cons (match-map-to-lst-helper (first lst) sat-map)
                (match-map-to-lst (rest lst) sat-map))]))
(define (match-map-to-lst-helper lst sat-map)
  (cond
    [(empty? lst) empty]
    [(even? (first lst))
     (cons (look-up (quotient (first lst) 2) sat-map)
           (match-map-to-lst-helper (rest lst) sat-map))]
    [(equal? 'u (look-up (quotient (first lst) 2) sat-map))
     (cons (look-up (quotient (first lst) 2) sat-map)
           (match-map-to-lst-helper (rest lst) sat-map))]
    [else (cons (not (look-up (quotient (first lst) 2) sat-map))
                (match-map-to-lst-helper (rest lst) sat-map))]))

(define (get-truth lst)
  (cond
    [(empty? lst) #t]
    [(member? 'u lst) #t]
    [(member? #t lst) #t]
    [else #f]))

(define (generate-sat-map elements)
  (cond
    [(empty? elements) empty]
    [else (cons (list (first elements) 'u)
                (generate-sat-map (rest elements)))]))

(define (update sat-map elem val)
  (cond
    [(empty? sat-map) -1]
    [(equal? (first (first sat-map)) elem)
     (cons (list elem val)
           (rest sat-map))]
    [else
     (cons (first sat-map)
           (update (rest sat-map) elem val))]))

;;-----------------------------------------------------------------------------------------------
(define (get-input-helper acc)
  (define x (read-line))
  (define (helper x)
    (cond
      [(equal? "" x) acc]
      [else
       (get-input-helper (append acc (to-list x)))]))
  (helper x))

(define (to-list x)
  (define lst (string->list x))
  (list (filter (lambda (y) (not (equal? #\space y))) lst)))

(define (get-map-master lst)
  (get-map (to-1d lst) empty empty 0))

(define (to-1d lst)
  (cond
    [(empty? lst) lst]
    [(append (first lst) (to-1d (rest lst)))]))

(define (get-map lst set map counter)
  (cond
    [(empty? lst) map]
    [(equal? #\~ (first lst)) (get-map (rest lst) set map counter)]
    [else
     (cond
       [(member? (first lst) set) (get-map (rest lst) set map counter)]
       [else
        (get-map (rest lst) (cons (first lst) set)
                 (cons (list (first lst) counter) map) (+ 1 counter))])]))

(define (get-elements-lst-master input-list acc)
  (get-elements-lst (to-1d input-list) empty))

(define (get-elements-lst lst acc)
  (cond
    [(empty? lst) acc]
    [(member? (first lst) acc) (get-elements-lst (rest lst) acc)]
    [else (get-elements-lst (rest lst) (cons (first lst) acc))]))


(define (process-clauses clauses acc map)
  (cond
    [(empty? clauses) acc]
    [else (process-clauses (rest clauses)
                           (cons (process-clause (first clauses) map) acc)
                           map)]))
(define (process-clause clause map)
  (cond
    [(empty? clause) empty]
    [(equal? (first clause) #\~)
     (cons (+ 1 (* 2 (look-up (second clause) map)))
           (process-clause (rest (rest clause)) map))]
    [else
     (cons (* 2 (look-up (first clause) map))
           (process-clause (rest clause) map))]))

(define (look-up x map)
  (cond
    [(empty? map) -1]
    [(equal? x (first (first map))) (second (first map))]
    [else (look-up x (rest map))]))

(define (member? elem lst)
  (cond
    [(empty? lst) #f]
    [(equal? elem (first lst)) #t]
    [else (member? elem (rest lst))]))

      

(define (print-my-list)
  (print (get-input-helper)))

#|
       
(define  x (read-line))
(printf x)

|#
