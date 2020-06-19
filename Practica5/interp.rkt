#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error' lookup "lookup: Hay un identificador libre: x")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)
  (type-case CFWBAE expr
    [id (i) (interp (lookup i ds) ds)]
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [iF (condicion then else)
        (if (boolV? condicion)
            ((if (equal? (interp condicion ds) (boolV #t))
             (interp then ds)
             (interp else ds)))
            (error "interp: Símbolo no esperado la condicional de if, no es un booleano"))
         ]
    [op (f l)
        (cond 
          [(equal? anD f) (if (allSameType boolV? l) (boolV (foldr myAnd #t (map (λ (expr) (boolV-b (interp expr ds))) l))) (error "interp: Los argumentos no son de un mismo tipo de dato"))]
          [(equal? oR f)  (if (allSameType boolV? l) (boolV (foldr myOr #t (map (λ (expr) (boolV-b (interp expr ds))) l))) (error "interp: Los argumentos no son de un mismo tipo de dato"))]
          [(equal? + f) (numV (foldr + 0 (map (λ (expr) (numV-n (interp expr ds))) l)))] 
          [(equal? - f) (numV (aux-op - (reverse (map (λ (expr) (numV-n (interp expr ds))) l))))] 
          [(equal? * f) (numV (foldl * 1 (map (λ (expr) (numV-n (interp expr ds))) l)))]   
          [(equal? / f) (numV (aux-op / (reverse (map (λ (expr) (numV-n (interp expr ds))) l))))]
          [(equal? > f) (boolV (> (numV-n (interp (first l) ds)) (numV-n (interp (second l) ds))))]
          [(equal? < f) (boolV (< (numV-n (interp (first l) ds)) (numV-n (interp (second l) ds))))]
          [(equal? >= f) (boolV (>= (numV-n (interp (first l) ds)) (numV-n (interp (second l) ds))))]
          [(equal? <= f) (boolV (<= (numV-n (interp (first l) ds)) (numV-n (interp (second l) ds))))]
          [(equal? = f) (boolV (= (numV-n (interp (first l) ds)) (numV-n (interp (second l) ds))))]
          [(equal? modulo f) (numV (modulo (numV-n (interp (first l) ds)) (numV-n (interp (second l) ds))))]
          [(equal? expt f) (numV (expt (numV-n (interp (first l) ds)) (numV-n (interp (second l) ds))))]
          [(equal? add1 f) (numV (add1 (numV-n (interp (first l) ds))))]
          [(equal? sub1 f) (numV (sub1 (numV-n (interp (first l) ds))))]
          [(equal? not f)  (boolV (not (boolV-b (interp (first l) ds))))]
          [(equal? zero? f)  (boolV (zero? (numV-n (interp (first l) ds))))]
          
          )]
    [fun (params body) (closure params body ds)]
    [app (foo args)  (interp (closure-body (interp foo ds )) (aux-env (closure-param (interp foo ds)) args (closure-env (interp foo ds))))]
    )
  )
(define (mysum l r)
  (+ l r)
  )
(define (myAnd l r)
  (and l r)
  )
(define (myOr l r)
  (or l r)
  )

(define (allSameType typeProcedure list)
  (if (empty? list) #t (if (typeProcedure (car list)) (allSameType typeProcedure (cdr list)) #f) )
  )

;;(foldr myOr #f (map (λ (expr) expr) '(#t #t #f ) ))
;;(foldr + 0 (map (λ (expr) expr) '(1 2 3 ) ))
;;(foldr (and #t) + #t (map (λ (expr) expr) '(#t #f #t ) ))
 ;; (interp (closure-body (interp foo ds )) (aux-env (closure-param (interp foo ds)) args (closure-env (interp foo ds))))]

;Esta funcion crea el nuevo ambiente con los parametros de la funcion y la lista de argumentos que recibe la aplicacion de funcion
; aux-env:  (list of symbol) (listof CFWAE) DefrdSub -> DefrdSub
(define (aux-env params args env)
  (if (empty? params) env
  (aux-env (cdr params) (cdr args) (aSub (car params) (car args) env))
  ))

;Funcion para arreglar unas operaciones truculentas
(define (aux-op op l) (if (empty? (cdr l)) (car l) (op (aux-op op (cdr l)) (car l))))

(define (prueba exp)
  (interp (desugar (parse exp)) (mtSub)))