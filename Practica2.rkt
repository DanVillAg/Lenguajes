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
;;(define (descomposicion-primos n))

;;Funcion que recibe n, r y devuelve el conjunto de multiplos n,
;;en el rango n y r.
;;multiplos: number number -> (listof number)
;;(define (multiplos n r))

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