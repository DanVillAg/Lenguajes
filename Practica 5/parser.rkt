#lang plai
(require (file "./grammars.rkt"))
;;(equal? (condS (list (condition (opS < (list (numS 2) (numS 3))) (numS 1)) (condition (opS >(list (numS 10) (numS 2))) (numS 2)) (else-cond (numS 3)))) (parse '{cond {{< 2 3} 1} {{> 10 2} 2} {else 3}}))
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
       [(iFS) (iFS (parse (car (cdr sexp))) (parse (car (cdr (cdr sexp)))) (parse (car (cdr (cdr (cdr sexp))))))]
       [(+) (opS + (for/list ((i (cdr sexp))) (parse i)))]
       [(-) (opS - (for/list ((i (cdr sexp))) (parse i)))]
       [(*) (opS * (for/list ((i (cdr sexp))) (parse i)))]
       [(/) (opS / (for/list ((i (cdr sexp))) (parse i)))]
       [(>) (opS > (for/list ((i (cdr sexp))) (parse i)))]
       [(<) (opS < (for/list ((i (cdr sexp))) (parse i)))]
       [(modulo) (opS modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(expt) (opS expt (list (parse (second sexp)) (parse (third sexp))))]
       [(add1) (opS add1 (list (parse (second sexp)))) ]
       [(sub1) (opS sub1 (list (parse (second sexp)))) ]
       [(cond) (condS (for/list ((i (cdr sexp))) (if (equal? (car i) 'else) (else-cond (parse (car (cdr i)))) (condition (parse (car i)) (parse (cdr (car i))) )) ))]
       [(withS) (withS (for/list ((i (second sexp))) (binding (first i) (parse (second i)))) (parse (third sexp)))]
       [(withS*) (withS* (for/list ((i (second sexp))) (binding (first i) (parse (second i)))) (parse (third sexp)))]
       [(funS) (funS (car (cdr sexp)) (parse (car (cdr (cdr sexp)))))] 
       [else (list? (car sexp)) (appS (funS (car (cdr (car sexp))) (parse (car (cdr (cdr (car sexp)))))) (for/list ((i (car (cdr sexp)))) (parse i)))]
       )]

  ))