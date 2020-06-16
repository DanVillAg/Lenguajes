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
      [(iFS) (iFS (parse (car (cdr sexp))) (parse (car (cdr (cdr sexp)))) (parse (car (cdr (cdr (cdr sexp))))))]
       [(+) (opS + (for/list ((i (cdr sexp))) (parse i)))]
       [(-) (opS - (for/list ((i (cdr sexp))) (parse i)))]
       [(*) (opS * (for/list ((i (cdr sexp))) (parse i)))]
       [(/) (opS / (for/list ((i (cdr sexp))) (parse i)))]
       [(modulo) (opS modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(expt) (opS expt (list (parse (second sexp)) (parse (third sexp))))]
       [(add1) (opS add1 (list (parse (second sexp)))) ]
       [(sub1) (opS sub1 (list (parse (second sexp)))) ]
       )]

  ))