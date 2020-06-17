#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar sexpr)
  (type-case SCFWBAE sexpr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [iFS (condition then else) (iF condition then else)]
    [opS (f l) (op f  (for/list ((i l)) (desugar i)))]
    [condS (cases) (toIF cases)]
    [withS (bind body) (app (fun (get-params bind) (desugar body))  (for/list ((i (get-args bind))) (desugar i))  )]
    [withS* (bind body) (desugar (toWITH bind body))]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (for/list ((i args)) (desugar i)) )]     
    )
  )

(define (toWITH bind body)
  (if (empty? bind) body (withS (list (car bind)) (toWITH (cdr bind) body))
  ))

;Esta funcion obtiene los parametros de with para la aplicacion de la funcion
(define (get-params l)
  (map (λ (bind) (binding-id bind)) l)
  )
;Esta funcion obtiene los argumentos de with para la aplicacion de la funcion
(define (get-args l)
  (map (λ (bind) (binding-value bind)) l)
  )


(define (toIF cases)
  (let ([c (car cases)]) 
  (if (condition? c) (iF (desugar (condition-test-expr c))  (desugar (condition-then-expr c)) (toIF (cdr cases)) )  (desugar (else-cond-else-expr c)))
  ))