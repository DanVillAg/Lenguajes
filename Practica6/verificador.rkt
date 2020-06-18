#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof SCFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
    [idS (i) 1]
    [numS (n) (numberT)]
    [boolS (b) (booleanT)]
    [iFS (condition then else) (if (equal? (typeof condition (phi)) (booleanT)) (if (equal? (typeof then (phi)) (typeof else (phi)))  (typeof then (phi)) (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr") ) (error "if: Type error\nConditional's type must be a boolean\nGiven: (numberT)" (typeof condition (phi)) ))]
    [opS (f l)
         (cond 
          [(equal? + f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? - f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? * f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? / f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? < f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? > f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? <= f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? >= f) (if (all (for/list ((i l)) (numberT? (typeof i (phi) )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? not f) (if (all (for/list ((i l)) (booleanT? (typeof i (phi) )))) (booleanT) (error ""))]
         )]
    [condS  (cases) (if (toType cases #t) (getType cases) (error "if: Type error\nConditional's type must be a boolean\nGiven: (numberT)"))]
    [withS  (bindings body) 1]
    [withS*  (bindings body) 1]
    [funS (params type body) (params body)]
    [appS (foo args) 1]
    ))

;;(prueba '{fun {{x : number 3} {y : boolean #f}} : number {if y x 0}})
;;(parse'{fun {{x : number 3} {y : boolean #f}} : number {if y x 0}})

(define (toType cases b)
  (let ([c (car cases)]) 
  (if (condition? c) (if  (equal? (typeof (condition-test-expr c) (phi)) (booleanT)) (toType (cdr cases) #t)  (error "if: Type error\nConditional's type must be a boolean\nGiven: (numberT)"))
  b)))

(define (getType cases)
  (let ([c (car cases)])
    (checkConditions (cdr cases) (typeof (condition-then-expr c) (phi)))
    )
  )

(define (checkConditions cases type)
  (let ([c (car cases)])
    (cond
      [(condition? c) (if  (equal? (typeof (condition-then-expr c) (phi)) type) (checkConditions (cdr cases) type)  (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr"))]
      [(else-cond? c) (if  (equal? (typeof (else-cond-else-expr c) (phi)) type) type  (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr"))]
      )
))


(define (all lst)
  (if (equal? lst '())  
      #t               
      (if (not (car lst))   
          #f
          (all (cdr lst)))))

#|
(define-type SCFWBAE
  [idS    (i symbol?)]
  [numS   (n number?)]
  [boolS  (b boolean?)]
  [iFS    (condicion SCFWBAE?) (then SCFWBAE?) (else SCFWBAE?)]
  [opS    (f procedure?) (args (listof SCFWBAE?))]
  [condS  (cases (listof Condition?))]
  [withS  (bindings (listof binding?)) (body SCFWBAE?)]
  [withS* (bindings (listof binding?)) (body SCFWBAE?)]
  [funS   (params (listof param?)) (rType Type?) (body SCFWBAE?)]
  [appS   (fun SCFWBAE?) (args (listof SCFWBAE?))])

|#
  
(define (prueba exp)
  (typeof (parse exp) (phi)))