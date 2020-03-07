#lang plai

;; ((λ (x) (+ x 2)) 4)


;;Funcion que recibe una lista y devuelve el producto cruz de los
;;elementos de dicha lista, consigo misma.
;;conjunto-cuardado: (listof number) -> (listof (pairof number))
;;(define (conjunto-cuadrado lista))


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

(define-type Figura
  [Circulo    (radio number?)]
  [Cuadrado   (lado  number?)]
  [Rectangulo (base number?) (altura number?)]
  [Triangulo  (base number?) (altura number?)])


;;Funcion que recibe una figura y calcula su perimetro.
;;perimetro: Figura -> number
;;(define (perimetro figura))

;;Funcion que recibe un numero, un arbol binario y agrega el elemento al
;;arbol de busqueda binario.
;;agrega: number ABB -> number
;;(define (agrega n arbol))


;;Funcion que recibe un arbol binario y calcula su altura.
;;altura: ABB -> number
;;(define (altura arbol))

;;Funcion que recibe un arbol binario, un elemento y
;;devuelve verdadero si el elemento esta contenido en el arbol,
;;falso en otro caso.
;;contiene: ABB ->  number -> boolean
;;(define (contiene arbol e))
