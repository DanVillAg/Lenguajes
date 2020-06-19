#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))
(require (file "./interp.rkt"))

;; Función auxiliar para pruebas, llama a parse y a interp
;; con el caché vacío.
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; test: A -> CFWBAE
(define (prueba exp)
  (interp (desugar (parse exp)) (mtSub)))

;Pruebas parse
(display "PRUEBAS PARSE\n______________________________________________________________________________________________________________________________\n\n")
(test (parse '{cond {{< 2 3} 1} {{> 10 2} 2} {else 3}}) (condS
                                                         (list
                                                         (condition (opS < (list (numS 2) (numS 3))) (numS 1))
                                                          (condition (opS >(list (numS 10) (numS 2))) (numS 2))
                                                          (else-cond (numS 3)))))
(test (parse '{cond {#t 1} {#f 2} {else 3}})
      (condS (list (condition (boolS #t) (numS 1)) (condition (boolS #f) (numS 2)) (else-cond (numS 3)))))

(test (parse '{cond {{not #f} 1} {#f 2} {else 3}})
      (condS (list (condition (opS not (list (boolS #f))) (numS 1)) (condition (boolS #f) (numS 2)) (else-cond (numS 3)))))

(test  (parse '{with {{x 5} {y 1}} {with {{z {+ x y 12}}} {+ z 0}}})
       (withS (list (binding 'x (numS 5)) (binding 'y (numS 1))) (withS (list (binding 'z (opS + (list (idS 'x) (idS 'y) (numS 12))))) (opS + (list (idS 'z) (numS 0))))))

(test (parse '{{fun {x y} {+ x y}} {10 3}}) (appS (funS '(x y) (opS + (list (idS 'x) (idS 'y)))) (list (numS 10) (numS 3))))

;Pruebas desugar
(display "PRUEBAS DESUGAR\n______________________________________________________________________________________________________________________________\n\n")
(test (desugar (parse '{cond {#t 1} {#f 2} {else 3}})) (iF (bool #t) (num 1) (iF (bool #f) (num 2) (num 3))))

(test (desugar (parse '{with {{x 5} {y 1}} {+ x y}}))
      (app (fun '(x y) (op + (list (id 'x) (id 'y)))) (list (num 5) (num 1))))

(test (desugar (parse '{with {{x 5} {y 1}} {with {{z {+ x y 12}}} {+ z 0}}}))
         (app (fun '(x y) (app (fun '(z) (op + (list (id 'z) (num 0)))) (list (op + (list (id 'x) (id 'y) (num 12)))))) (list (num 5) (num 1))))


(test (desugar (appS (funS '(x y) (opS + (list (idS 'x) (idS 'y)))) (list (numS 10) (numS 3))))
      (app (fun '(x y) (op + (list (id 'x) (id 'y)))) (list (num 10) (num 3))))

;Pruebas interp
(display "PRUEBAS INTERP\n______________________________________________________________________________________________________________________________\n\n")
(test (prueba '{cond {{< 2 3} 1} {{> 10 2} 2} {else 3}}) (numV 1));;

(test (prueba '{cond {{= 2 3} 1} {{> 10 2} 2} {else 3}}) (numV 2));;

(test (prueba '{cond {#f 1} {#f 2} {else 3}}) (numV 3)) ;;

(test (prueba '3) (numV 3)) ;;

(test (prueba '#t) (boolV #t)) ;;

(test (prueba'{not #t}) (boolV #f));;

(test (prueba'{not #f}) (boolV #t));;

(test (prueba '{< 2 3}) (boolV #t));;

(test (prueba '{< 3 2}) (boolV #f));;

(test (prueba '{> 2 3}) (boolV #f));;

(test (prueba '(if {not #t} 1 3)) (numV 3));;

(test (prueba '(if {not {< 4 3}} 1 3)) (numV 1));;

(test (prueba '(cond {{not {< 4 3}} 1} {#f 2} {else 3})) (numV 1));;

(test (prueba '(cond {{not {< 2 3}} 1} {(< 3 4) 2} {else 3})) (numV 2));;

(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6));

(test (prueba '{with* {{x 5} {y x}} {+ x y}}) (numV 10));

(test (prueba'{with {{x 5} {y 1}} {with {{z {+ x y 12}}} {+ z 0}}}) (numV 18));

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11));

;(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}}) "lookup: Hay un identificador libre: x")

(test (prueba '{{fun {x y} {+ x y}} {10 3}}) (numV 13));

(test (prueba '{with* {{x 1} {y 2} {z 3}} {fun {x y x} {+ x {+ y z}}}})
      (closure '(x y x) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (aSub 'z (num 3) (aSub 'y (num 2) (aSub 'x (num 1) (mtSub)))))) ;;

;(test/exn (prueba '{if 2 5 6}) "interp: Símbolo no esperado la condicional de if, no es un booleano")
#|(test (prueba '{with* {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f {1}}}}}) (numV 4))
(test (prueba '{with {{f {fun {x} {+ x x}}}} {f {3}}}) (numV 6))

(test (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f {0}}}) "lookup: Hay un identificador libre: x")
|#

(display "OTROS:\n_____________________________________________________________________________________________________________________________________________________\nPRUEBAS PROBADAS\n\n\n")








(test (interp (desugar (parse '3)) (mtSub)) (numV 3))

(test (interp (desugar (parse #t)) (mtSub)) (boolV #t))


(test (interp (desugar (parse '{cond {#t 1} {#f 2} {else 3}})) (mtSub))
      (numV 1))

(test (interp (desugar (parse '{cond {#f 1} {#t 2} {else 3}})) (mtSub))
      (numV 2))

(test (interp (desugar (parse '{cond {#t 1} {#t 2} {else 3}})) (mtSub))
      (numV 1))

(test (interp (desugar (parse '{cond {#f 1} {#f 2} {else 3}})) (mtSub))
      (numV 3))

(test (interp (desugar (parse '(+ 1 1 1 (- 3 4 1) (sub1 1)))) (mtSub))
      (numV 1))

(test (prueba '{with {{a 2}} {sub1 a}}) (numV 1))

(test (prueba '(with* ((x 1) (y 2)) (+ y x))) (numV 3))


(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6))

(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) (numV 6)) 

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11))





(test (prueba '{with* {{x 1} {y 2} {z 3}} {fun {x y x} {+ x {+ y z}}}})
      (closure '(x y x)
               (op + (list (id 'x) (op + (list (id 'y) (id 'z)))))
               (aSub 'z (num 3)
                     (aSub 'y (num 2) (aSub 'x (num 1) (mtSub))))))

(test (prueba '{with {{x 5}} {+ x x}})
      (numV 10))
                      

(test (prueba '{with {{x {+ 5 5}}} {+ x x}})
      (numV 20))

(test (prueba '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})
      (numV 14))

(test (prueba '{with {{x 5} {y {- 5 3}}} {+ x y}})
      (numV 7))

(test (prueba '{with {{x 5}} {+ x {with {{x 3}} 10}}})
      (numV 15))

(test (prueba '{with {{x 5}} {+ x {with {{y 3}} x}}})
      (numV 10))

(test (prueba '{with {{x 5}} {+ x {with {{x 3}} x}}})
      (numV 8))

(test (prueba '{with {{x 5}} {with {{y x}} y}})
      (numV 5))





(test (prueba '{with {{x 3}} {fun {y} {+ x y}}})
      (closure '(y) (op + (list (id 'x) (id 'y)))
               (aSub 'x (num 3) (mtSub))))


(test (prueba '{or #f #f #f #f}) (boolV #f))

(test (prueba '{or #f #f {or #t #f} #f}) (boolV #t))

(test (prueba '(not (or #f #f (or #t #f) #f))) (boolV #f))

(test (prueba '{and #t {or #t #f} #t}) (boolV #t))


(test/exn (interp (desugar (parse 'x)) (mtSub))
          "lookup: Hay un identificador libre: x")
#|
(test (prueba '{with* {{x 5}
                       {w {+ x 1}}
                       {z {with {{x 10}
                                 {f {fun {a} {+ x a}}}}
                                {f 10}}}}
                       {+ x z}}) (numV 20))

(test (prueba '{with* {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f 1}}}}) (numV 4))

(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}})
          "lookup: Hay un identificador libre: x")

(test (prueba '{{fun {x y} {+ x y}} 10 3}) (numV 13))

(test (prueba '{with {{f {fun {x} {+ x x}}}} {f 3}}) (numV 6))


(test/exn (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f 0}}) "lookup: Hay un identificador libre: x")

(test (prueba '{with* {{x 2} {y {+ x x}} {x 1}} {+ 0 y}})
      (numV 4))
(test (prueba '{with* {{x 2} {y {+ x x}} {x 1}} {+ x y}})
      (numV 5))

(test (prueba '{with {{x 5}} {with {{x x}} x}})
      (numV 5))

(test (prueba '{{fun {x} x} 3}) (numV 3))



(test/exn (prueba '{with {{x 5} {f {fun {y} {+ x y}}}} {f 10}})
          "lookup: Hay un identificador libre: x")

(test/exn (prueba '{or #t #f #t 1})
          "interp: Los argumentos no son de un mismo tipo de dato")


|#