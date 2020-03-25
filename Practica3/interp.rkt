#lang plai
(require (file "./parser.rkt"))
(require (file "./grammars.rkt"))

;; Recibe una expresión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id value)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id ) value expr)]
    [num (n) expr ]
    [op (f l) (op f (for/list ((i l)) (subst i sub-id value)))]
    [with (bind body) 2]
    [with* (bind body) 3]
    ))


;; Toma un árbol de sintáxis abstraca del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number
(define (interp expr)
  (type-case WAE expr
    [id (i) (error 'interp "Identificador libre >:")]
    [num (n) n]
    [op (f l)
        (cond
          [(equal? + f) (foldr + 0 (map (λ (expr) (interp expr)) l))]
          ;;[(equal? - f) (foldl - 0 (map (λ (expr) (interp expr)) l))]
          [(equal? * f) (foldl * 1 (map (λ (expr) (interp expr)) l))]
          ;;[(equal? / f) (foldr / 0 (map (λ (expr) (interp expr)) l))]
          [(equal? modulo f) (modulo (interp (first l)) (interp (second l)))]
          [(equal? expt f) (expt (interp (first l)) (interp (second l)))]
          [(equal? add1 f) (add1 (interp (first l)))]
          [(equal? sub1 f) (sub1 (interp (first l)))]
          )]
    [with (bind body) 2]
    [with* (bind body) 3]
    ))
;;(foldr + 0 (map (λ (expr) (interp expr)) l))
;;Ejemplo (foldr + 0 (map (λ (expr) (interp expr)) (list (num 1) (num 2) (num 3))))
;;Ehemplo (interp (op + (list (num 1) (num 2) (num 3))))