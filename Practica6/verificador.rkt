#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof SCFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
    [idS (i) (contextLookup i context)]
    [numS (n) (numberT)]
    [boolS (b) (booleanT)]
    [iFS (condition then else) (if (equal? (typeof condition context) (booleanT)) (if (equal? (typeof then context) (typeof else context))  (typeof then context) (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr") ) (error "if: Type error\nConditional's type must be a boolean\nGiven: (numberT)" (typeof condition context) ))]
    [opS (f l)
         (cond
          [(equal? anD f) (if (all (for/list ((i l)) (booleanT? (typeof i context )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? oR f)  (if (all (for/list ((i l)) (booleanT? (typeof i context )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? + f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? - f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? * f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? / f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (numberT) (error "typeof: Error in parameter (boolS #f)\n Expected type: (numberT)\nGiven type: (booleanT)"))]
          [(equal? < f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? > f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? <= f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? >= f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (booleanT) (error "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)"))]
          [(equal? not f) (if (all (for/list ((i l)) (booleanT? (typeof i context )))) (booleanT) (error ""))]
          [(equal? zero? f) (if (all (for/list ((i l)) (numberT? (typeof i context )))) (booleanT) (error ""))]
         )]
    [condS  (cases) (if (toType cases #t context) (getType cases context) (error "if: Type error\nConditional's type must be a boolean\nGiven: (numberT)"))]
    [withS  (bindings body)
            (let ([next-context (addWithContext bindings context)])
              (typeof body next-context)
              )]
    [withS*  (bindings body) (let ([next-context (addWithContext bindings context)])
              (typeof body next-context)
              )]
    [funS (params type body)
          (let ([next-context (addContext params context)])
          (if (equal? type (typeof body next-context)) (funT (append (for/list ((i params)) (param-tipo i)) (list type))) (error 'typeof (string-append "Error in funT type\nExpected type:" (~v(type)) "\nGiven type:" (~v(typeof body next-context)) )) ))] ;; (funT (append (for/list ((i params)) (param-tipo i)) (list type)))]
    [appS (foo args) (if (check-real-args (funS-params foo) args context) (car (reverse (funT-params (typeof foo context)) )) (error "app: Type error:\nParameter's type doesn't match expected types\nGiven: (numberT)\nExpected: (booleanT)") )]
    ))

(define (addWithContext binding existing-context)
  (if (empty? binding) existing-context (addWithContext (cdr binding) (gamma (binding-id (car binding)) (binding-tipo (car binding)) existing-context)) )
  )

(define (check-real-args params args context)
  (let ([expected (for/list ((i params)) (param-tipo i))] [given (for/list ((i args)) (typeof i context))])
    (if (equal? given expected) #t #f)))

;; como hacer comentarios para errores, según lo visto en la grabación del laboratorio:
;; (error 'typeof (string-append "Error in funT type\nExpected type:" (~v(funcion-que-devuelde-el-tipo)) "\nGiven type:" (~v(funcion-que-devuelde-el-tipo)) ))

(define (addContext params existing-context)
  (if (empty? params) existing-context (addContext (cdr params) (gamma (param-param (car params)) (param-tipo (car params)) existing-context)) )
  )

;;CON ESTE LOOKUP TODAS LAS FUNCIONES QUE REVISAN EL TYPEOF LLEVAN EL CONTEXTO GLOBAL, ESO HACE AL VERIFICADOR DE EVALUACIÓN DINÁMICA
;; Busca el identificador "name" en el contexto 
;;  "context" regresando el tipo correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol Type-Context -> Type
(define (contextLookup name context)
  (type-case Type-Context context
    [phi() (error' contextLookup "lookup: Hay un identificador libre: x")]
    [gamma (bound-name bound-type rest-context)
          (if (symbol=? bound-name name)
              bound-type
              (contextLookup name rest-context))]))

#|
(define-type Type-Context
  [phi]
  [gamma (id symbol?)  (tipo Type?) (rest Type-Context?)])
|#


;;(prueba '{fun {{x : number 3} {y : boolean #f}} : number {if y x 0}})
;;(parse'{fun {{x : number 3} {y : boolean #f}} : number {if y x 0}})

(define (toType cases b context)
  (let ([c (car cases)]) 
  (if (condition? c) (if  (equal? (typeof (condition-test-expr c) context) (booleanT)) (toType (cdr cases) #t)  (error "if: Type error\nConditional's type must be a boolean\nGiven: (numberT)"))
  b)))

(define (getType cases context)
  (let ([c (car cases)])
    (checkConditions (cdr cases) (typeof (condition-then-expr c) context) context)
    )
  )

(define (checkConditions cases type context)
  (let ([c (car cases)])
    (cond
      [(condition? c) (if  (equal? (typeof (condition-then-expr c) context) type) (checkConditions (cdr cases) type)  (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr"))]
      [(else-cond? c) (if  (equal? (typeof (else-cond-else-expr c) context) type) type  (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr"))]
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