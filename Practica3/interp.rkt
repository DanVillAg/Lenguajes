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
    [with  (bind body) (with bind   (if [member sub-id (map binding-id bind)] body (subst body sub-id value)))]
    [with* (bind body) (with* bind  (if [member sub-id (map binding-id bind)] body (subst body sub-id value)))]
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
          [(equal? + f) (foldr + 0 (map (λ (expr) (interp expr)) l))] ;;(map (λ (expr) (interp expr)) l) Genera lista
          ;;[(equal? - f) (foldl - 0 (map (λ (expr) (interp expr)) l))] ;; por ejemplo (list (num 2) (num 3) (num 4) ->
          [(equal? * f) (foldl * 1 (map (λ (expr) (interp expr)) l))]    ;; es igual a  (list 2 3 4)
          ;;[(equal? / f) (foldr / 0 (map (λ (expr) (interp expr)) l))]
          [(equal? modulo f) (modulo (interp (first l)) (interp (second l)))]
          [(equal? expt f) (expt (interp (first l)) (interp (second l)))]
          [(equal? add1 f) (add1 (interp (first l)))]
          [(equal? sub1 f) (sub1 (interp (first l)))]
          )]
    [with  (bind body) (interp (interp-aux bind body))]
    [with* (bind body) (interp (interp-aux bind body))]
    ))
(define (interp-aux bind body)
  (if (empty? bind) body (interp-aux (cdr bind) (subst body (binding-id (car bind)) (binding-value (car bind))))))


;;(foldr + 0 (map (λ (expr) (interp expr)) l))
;;Ejemplo (foldr + 0 (map (λ (expr) (interp expr)) (list (num 1) (num 2) (num 3))))
;;Ejemplo (interp (op + (list (num 1) (num 2) (num 3))))
;;Ejemplo (subst (parse '{with {{a 2} {b 3}} {+ b c 3}}) 'c (num 7))


