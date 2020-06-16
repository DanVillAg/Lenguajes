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
    [opS (f l) (op f l)]
    [condS (cases) #t]
    [withS (bind body) #t]
    [withS* (bind body) #t]
    [funS (params body) #t]
    [appS (fun args) #t]     
    )
  )