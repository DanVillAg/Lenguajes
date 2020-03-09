#lang plai

;; ((λ (x) (+ x 2)) 4)


;;Funcion que recibe una lista y devuelve el producto cruz de los
;;elementos de dicha lista, consigo misma.
;;conjunto-cuardado: (listof number) -> (listof (pairof number))

(define (conjunto-cuadrado lista)
  (remove-duplicates (filter (λ (x) (not (empty? x))) (conjunto-aux1 lista lista)))
  )


(define (conjunto-aux1 lista lis)
  (let ([cart
  (for/list ([i lista])
     (if (empty? lis) '()   (list (car lis) i))
    )]) (if (empty? lis) (append cart '())  (append cart  (conjunto-aux1 lista (cdr lis))))
    )
  )

;;Funcion que calcula el cambio que tenemos que devolver segun el
;;monto a cobrar y el monto pagado. Devuevel la cantidad de monedas de las
;;denominaciones $50, $20,  $10, $5, $2, $1.
;;area-cono: number number -> (number number number number number number)
;;(define (cambio total pago))

;;Funcion que calcula la descomposicion en factores primos de un numero
;;descomposicion-primos: number -> (listof (pairof number))
(define (descomposicion-primos n)
  (empareja (factores n '() 2 n))
 )

(define (empareja lista)
  (let ([x (longitud (filter (λ (n)(equal? (car lista) n)) lista))])
    (if (empty? lista) '()
        (append (list (list (car lista) x)) (empareja (filter (λ (n) (not (equal? n (car lista)))) lista)) ))
  )
)
(define (factores n lista ld i)
  (cond
   [(equal? n 1) lista]
   [(> ld i) (append lista (list i))]
   [(divisor? 2 n) (factores (/ n 2) (append lista '(2)) 2 i)]
   [(divisor? 3 n) (factores (/ n 3) (append lista '(3)) 3 i)]
   [else (if (equal? (modulo n (+ 2 ld)) 0)
         (factores (/ n (+ 2 ld)) (append lista (list (+ 2 ld))) ld i)
         (factores n lista (+ 2 ld) i))]
   )
  )
  

;;Funcion que recibe n, r y devuelve el conjunto de multiplos n,
;;en el rango n y r.
;;multiplos: number number -> (listof number)
(define (multiplos n r) (filter (λ (x) (< x r)) ) (filter (λ (x) (not (zero? x)))  (for/list ([i r]) (* i n)))) )

;;Definicion del tipo Figura
(define-type Figura
  [Circulo    (radio number?)]
  [Cuadrado   (lado  number?)]
  [Rectangulo (base number?) (altura number?)]
  [Triangulo  (base number?) (altura number?)])


;;Funcion que recibe una figura y calcula su perimetro.
;;perimetro: Figura -> number
(define (perimetro figura)
  (type-case Figura figura
    [Circulo (r) (* (* 2 r) pi)]
    [Cuadrado (l) (* 4 l)]
    [Rectangulo (b a) (+ (* 2 b) (* 2 a))]
    [Triangulo (b a ) (* 3 b)]
      )
  )

;;Funcion que recibe una figura y calcula su area.
;;area: Figura -> number
(define (area figura)
  (type-case Figura figura
    [Circulo (r) (* pi (potencia r 2))]
    [Cuadrado (l) (* l l)]
    [Rectangulo (b a) (* b a)]
    [Triangulo (b a) (/ (* b a) 2)]
      )
  )

;;Definicion del tipo ABB
(define-type ABB
  [vacio]
  [hoja (e number?)]
  [nodo (e number?) (izq ABB?) (der ABB?)]
  )

;;Funcion que recibe un numero, un arbol binario y agrega el elemento al
;;arbol de busqueda binario.
;;agrega: number ABB -> number
(define (agrega n arbol)
  (type-case ABB arbol
    [vacio () (hoja n)]
    [hoja (e) (nodo e vacio (hoja n))]
    [nodo (e i d) (vacio)]
  )
)

;;Funcion que recibe un arbol binario y calcula su altura.
;;altura: ABB -> number
;;(define (altura arbol))

;;Funcion que recibe un arbol binario, un elemento y
;;devuelve verdadero si el elemento esta contenido en el arbol,
;;falso en otro caso.
;;contiene: ABB ->  number -> boolean
;;(define (contiene arbol e))

;;Funciones Auxiliares:

;;Funcion que eleva el numero a, a la potencia b
;; potencia: number number
(define (potencia a b)
  (cond
    [(and (equal? a 0) (< b 0))  (error 'err "0 con exponente negativo no esta definido")]
    [(equal? b 0) 1]
    [(> b 0) (* a (potencia a (sub1 b)))]
    [(< b 0) (* (/ 1 a) (potencia a (add1 b)))]
))




;;Predicado que nos dice si un numero m es divisor de otro numero n
;;divisor?: number number -> number
(define (divisor? m n)
  (cond
    [(and (equal? n 0)(equal? m 0)) error 'bad "No se puede dividir 0 entre 0 :v"]
    [(equal? n 0) #t]
    [(integer? (/ n m)) #t]
    [(not (integer? (/ n m))) #f]
  ))

;;Funcion que nos da la longitud de una lista
;; longitud: (listof a) -> number
(define (longitud lista)
  (if (null? lista) 0 (+ 1 (longitud (cdr lista))))
  )