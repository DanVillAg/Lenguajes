#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (car sexp)
       [(+) (op + (for/list ((i (cdr sexp))) (parse i)))]
       [(-) (op - (for/list ((i (cdr sexp))) (parse i)))]
       [(*) (op * (for/list ((i (cdr sexp))) (parse i)))]
       [(/) (op / (for/list ((i (cdr sexp))) (parse i)))]
       [(modulo) (op modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(expt) (op expt (list (parse (second sexp)))) ]
       [(add1) (op add1 (list (parse (second sexp)))) ]
       [(sub1) (op sub1 (list (parse (second sexp)))) ]
       [(with) (with (for/list ((i (second sexp))) (binding (first i) (parse (second i)))) (parse (third sexp)))]
       )]
    ))

;;(with (list (binding 'a (num 3)) (binding 'c (num 7))) (id 'a))
(for/list ((i (second '{with {{a 2} {b 3}} {+ a b}})))
  (binding (first i) (parse (second i))))
