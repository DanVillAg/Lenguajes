#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
 (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(if) (iFS (parse (car (cdr sexp))) (parse (car (cdr (cdr sexp)))) (parse (car (cdr (cdr (cdr sexp))))))]
       [(or)  (if (aux-or (cdr sexp) #f) (parse #t) (parse #f))]
       [(and) (if (aux-and (cdr sexp) #t) (parse #t) (parse #f))]
       [(+) (opS + (for/list ((i (cdr sexp))) (parse i)))]
       [(-) (opS - (for/list ((i (cdr sexp))) (parse i)))]
       [(*) (opS * (for/list ((i (cdr sexp))) (parse i)))]
       [(/) (opS / (for/list ((i (cdr sexp))) (parse i)))]
       [(>) (opS > (for/list ((i (cdr sexp))) (parse i)))]
       [(<) (opS < (for/list ((i (cdr sexp))) (parse i)))]
       [(>=) (opS >= (for/list ((i (cdr sexp))) (parse i)))]
       [(<=) (opS <= (for/list ((i (cdr sexp))) (parse i)))]
       [(=) (opS = (for/list ((i (cdr sexp))) (parse i)))]
       [(modulo) (opS modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(expt) (opS expt (list (parse (second sexp)) (parse (third sexp))))]
       [(add1) (opS add1 (list (parse (second sexp)))) ]
       [(sub1) (opS sub1 (list (parse (second sexp)))) ]
       [(not)  (opS not  (list (parse (second sexp)))) ]
       [(cond) (condS (for/list ((i (cdr sexp))) (if (equal? (car i) 'else) (else-cond (parse (car (cdr i)))) (condition (parse (car i)) (parse (car (cdr  i)))  )) ))]
       [(with) (withS (for/list ((i (second sexp))) (binding (first i) (parse (second i)))) (parse (third sexp)))]
       [(with*) (withS* (for/list ((i (second sexp))) (binding (first i) (parse (second i)))) (parse (third sexp)))]
       [(fun) (funS (car (cdr sexp)) (parse (car (cdr (cdr sexp)))))] 
       [else (list? (car sexp)) (appS (funS (car (cdr (car sexp))) (parse (car (cdr (cdr (car sexp)))))) (for/list ((i (car (cdr sexp)))) (parse i)))]
       )]

  ))

(define (aux-or l b)
   (if (empty? l) b (if (boolean? (car l)) (aux-or (cdr l) (or (car l) b)) (aux-or (cdr (car l)) b))))
(define (aux-and l b)
   (if (empty? l) b (if (boolean? (car l)) (aux-and (cdr l) (and (car l) b)) (aux-and (cdr (car l)) b)))) 
 