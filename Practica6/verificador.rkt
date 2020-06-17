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
    [iFS (condicion then else) (if (equal? (typeof condition (phi)) booleanT) (1) (error "typeof: Error in parameter condition\n Expected type: (booleanT)\nGiven type: " (typeof condition (phi)) ))]
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
    [condS  (cases) 1]
    [withS  (bindings body) 1]
    [withS*  (bindings body) 1]
    [funS (params type body) (params body)]
    [appS (foo args) 1]
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