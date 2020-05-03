#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (car sexp)
       [(if0) (if0 (parse (car (cdr sexp))) (parse (car (cdr (cdr sexp)))) (parse (car (cdr (cdr (cdr sexp))))))]
       [(+) (op + (for/list ((i (cdr sexp))) (parse i)))]
       [(-) (op - (for/list ((i (cdr sexp))) (parse i)))]
       [(*) (op * (for/list ((i (cdr sexp))) (parse i)))]
       [(/) (op / (for/list ((i (cdr sexp))) (parse i)))]
       [(modulo) (op modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(expt) (op expt (list (parse (second sexp)) (parse (third sexp))))]
       [(add1) (op add1 (list (parse (second sexp)))) ]
       [(sub1) (op sub1 (list (parse (second sexp)))) ]
       [(with) (with (for/list ((i (second sexp))) (binding (first i) (parse (second i)))) (parse (third sexp)))]
       [(with*) (with* (for/list ((i (second sexp))) (binding (first i) (parse (second i)))) (parse (third sexp)))]
       [(fun) (fun (car (cdr sexp)) (parse (car (cdr (cdr sexp)))))] 
       [(app) (app (parse (car (cdr sexp))) (for/list ((i (car (cdr (cdr sexp))))) (parse i)))]
       )]
    [else (error "Error de Sintaxis")]
    ))
