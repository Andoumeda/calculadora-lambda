#lang racket
(require racket/trace)

;;Funcion principal: ejecuta esto para empezar
;; (Probablemente no hace falta modificar esta funcion)
(define(inicio )
  (cond [(null? (imprimir(evaluar(validar-sintaxis(leer)))))null]
        [else inicio]))
;;Funcion encargada de imprimir la expresion
;;
(define (imprimir expresion calculo)
	(write expresion)
        (write "  ")
        (write calculo)
        (displayln "")
  expresion)

;; funcion  encargada de validar la sintaxis de la expresion
;;
(define(validar-sintaxis expresion)
  (cond [(null? expresion) null]
        [(not (list? expresion))  null]
        [(empty? expresion) null]
        [(eqv? 'L (car expresion)) (validar-aux expresion)]
        [else null]))

(define (validar-aux expresion)
  (cond [(< (tamano expresion) 4) null]
        [(not (equal? 1 (contar-ocurrencias expresion '_))) null]
        [(not (equal? 1 (contar-ocurrencias expresion 'L))) null]
        [(equal? 0 (contar-variables-ligadura expresion)) null]
        [(eqv? '_ (list-ref expresion (- (tamano expresion) 1))) null]
        [(eqv? (validar-variables expresion) #t) (validar-cuerpo expresion)]
        [else null]))

(define ( validar-variables expresion )
  (cond [ (empty? expresion) null ]
        [ (eqv? (car expresion) '_) #t]
        [ (symbol? (car expresion)) (validar-variables (cdr expresion))]
        [ else null]))

(define (validar-cuerpo expresion)
  (cond [(null? expresion) #t]
        [(equal? 1 (contar-ocurrencias expresion '_)) ( validar-cuerpo (cdr expresion))]
        [(and(list? (car expresion))) (validar-cuerpo-aux (car expresion)) ]
        [else ( validar-cuerpo (cdr expresion))]))

(define (validar-cuerpo-aux expresion)
  (cond [ (null? expresion) null]
        [ (eqv? #t (validar-sintaxis expresion)) #t]
        [ (and (equal? 0 (contar-ocurrencias expresion 'L))
               (equal? 0 (contar-ocurrencias expresion '_))) #t]
        [else null]))

(define (contar-ocurrencias expresion elem)
  (foldl + 0 (map (lambda (x)1) (filter (lambda (x) (eqv? elem x)) expresion))))

(define (tamano lista)
  (cond
        [(not(list? lista)) null]
        [(null? lista) 0]
        [else (+ 1 (tamano (cdr lista)))]))

(define (contar-variables-ligadura expresion)
  (cond [(eqv? (car expresion) 'L) (contar-variables-ligadura (cdr expresion))]
        [(eqv? (car expresion) '_) 0]
        [else (+ 1 (contar-variables-ligadura (cdr expresion)))]))

;; funcion que dado un numero devuelve la formula de dado numero (solo positivos)
;; (ej: 1) por su formula (ej: (L f _ (L x _ (f x))) )
;;
(define (numero-a-formula numero)
  (cond [(not (number? numero)) null]
        [(not (> numero 0)) null]
        [else  (cons 'L (cons 'f (cons'x (cons'_ (numero-a-formula-aux numero 0)))))]
        )
  )

(define (numero-a-formula-aux numero contador)
  (cond [(equal? contador numero) (list 'x)]
        [else (cons  'f (numero-a-formula-aux numero (add1 contador)))]
        )
  )

(define (numero-a-formula2 numero)
  (cond [(not (number? numero)) null]
        [(< numero 0) null]
        [else (list 'L 'f '_ (list 'L 'x '_ (numero-a-formula-aux2 numero 0)))]
        )
  )

(define (numero-a-formula-aux2 numero contador)
  (cond [(> (+ contador 1) numero) list 'x]
        [else (list  'f (numero-a-formula-aux2 numero (add1 contador)))]
        )
  )


;; convierte la formula a un numero
;; Ej:
;; (L f _ (L x _ (f x))) por el numero 1
;; o tambien
;; (L y _ (L p _ (y p))) por el numero 1
;;
;; (es la estructura que importa no el nombre de las variables!!)
;;

(define (formula-a-numero formula)
  (cond[(null? formula) null]
       [(equal? (validar-sintaxis formula) #t) (calcular-numero formula)]
       [ else formula ]
       )
  )
(define (calcular-numero formula)
  (cond[(and (equal? (validar-sintaxis (car (obtener-cuerpo formula))) #t)
             (< 1 (contar-variables-ligadura (car (obtener-cuerpo formula))))) null]
       [(and (equal? 2 (contar-variables-ligadura formula))
             (equal? (validar-sintaxis (car (obtener-cuerpo formula))) #t)) null]
       [(and (equal? (validar-sintaxis (car(obtener-cuerpo formula))) #t)
             (equal? (validar-sintaxis (car (obtener-cuerpo (car (obtener-cuerpo formula))))) #t)) null]
       [else (calcular-numero-aux (obtener-cuerpo formula) (cadr formula) 0 )]))

(define (obtener-cuerpo formula)
  (cond[(equal? 0 (contar-ocurrencias formula '_)) formula]
       [ else (obtener-cuerpo (cdr formula))])
  )

(define (calcular-numero-aux formula char ocurr)
  (cond [(null? formula) ocurr]
        [(equal? (validar-sintaxis formula) #t) (calcular-numero-aux (obtener-cuerpo formula) char ocurr)]
        [(equal? char (car formula)) (calcular-numero-aux (cdr formula) char (add1 ocurr))]
        [(list? (car formula)) (calcular-numero-aux (car formula) char ocurr)]
        [else (calcular-numero-aux (cdr formula) char ocurr)]
   )
  )


;; recorre la expresion y sustituye todos los numeros por formulas

(define (sustituir-numeros expresion)
	(cond [(null? expresion) null]
	      [(number? (car expresion)) (cons (numero-a-formula2 (car expresion)) (sustituir-numeros (cdr expresion)))]
              [(list? (car expresion)) (cons (sustituir-numeros (car expresion)) (sustituir-numeros (cdr expresion)))]
              [else (cons (car expresion) (sustituir-numeros (cdr expresion)))]))

;; funcion para hacer reduccion beta
;; Ej: (L x _ (x x)) 2 ==> (2 2)
;; realiza 1 sola reduccion

(define (reduccion-beta expresion)
  (cond [(null? (reduccion-beta-aux expresion)) null]
        [(not(list? expresion)) expresion]
        [(list? (car (reduccion-beta-aux expresion))) (car (reduccion-beta-aux expresion))]
        [else (reduccion-beta-aux expresion)]))

(define (reduccion-beta-aux expresion)
        (cond [(null? expresion) null]
              [(not(list? expresion)) expresion]
              [(> 2 (tamano expresion)) expresion]
              [(eqv? null (validar-sintaxis (car expresion))) expresion ]
              [(eqv? null (validar-cuerpo (cdr expresion))) ]
              [(eqv? (caddar expresion) '_) (reducir (obtener-cuerpo (car expresion)) (cadar expresion) (cadr expresion))]
              [else (reducir (remove (cadar expresion) (car expresion)) (cadar expresion) (cadr expresion)) ]))

(define (reducir expresion ocurr arg)
  (cond [(null? expresion) null]
        [(equal? (car expresion) ocurr) (cons arg (reducir (cdr expresion) ocurr arg))]
        [(list? (car expresion))
         (if (and (equal? #t (validar-sintaxis (car expresion)))
                  (not(equal? 0 (contar-ocurrencias (obtener-definicion (car expresion)) ocurr))))
             (cons (car expresion) (reducir (cdr expresion) ocurr arg))
             (cons (reducir (car expresion) ocurr arg) (reducir (cdr expresion) ocurr arg)))]
        [else (cons (car expresion) (reducir (cdr expresion) ocurr arg))]))

;; funcion encargada de evaluar la expresion y obtener un resultado
;; combina todas las funciones anteriores para sustituir, luego hacer reducciones
;; hasta obtener un resultado simplificado.. y finalmente sustituye formulas por
;; numeros.
;;
;; La funcion tambien imprime en cada paso lo que hizo, e indica la regla que
;; utilizo
;;
;; Ej:
;;     (((L x _ (L y _ (x y))) 1) 2)
;;
;; La funcion imprime (como efecto secundario) a la pantalla:
;;     (((L x _ (L y _ (x y))) (L f _ (L x _ (f x)))) 2)    [sust-num]
;;     (((L x _ (L y _ (x y))) (L f _ (L x _ (f x)))) (L f _ (L x _ (f (f x))))) [sust-num]
;;     ((L y _ ((L f _ (L x _ (f x))) y)))  (L f _ (L x _ (f (f x)))))    [red-beta]
;;     (L y _ ((L f _ (L x _ (f x))) (L f _ (L x _ (f (f x)))))))      [red-beta]
;;     (L y _ ((L f _ (L x _ (f x))) (L ff _ (L xx _ (ff (ff xx)))) )))   [sust-variables x -> xx y f --> ff]
;;     (L y _ (L x _ (L ff _ (L xx _ (ff (ff xx)) )) x))    [red-beta]
;;     (L y _ (L x _ (L xx _ (x (x xx)))))  [red-beta]
;;     (L y _ (L x _ (L xx _ (x (x xx)))))  [red-beta]
;;     (L y _  2)   [sust-formula  (L x _ (L xx _ (x (x xx))))--> 2]
;;
;; Pero devuelve:
;;     (L y _ 2)
;;

(define (evaluar expresion)
  (cond [(null? expresion) null]
        [(not(pair? expresion)) (imprimir expresion "resultado")]
        [else (evaluar-aux
               (imprimir (SETF(SETF
                          (imprimir (sustituir-numeros
                                     (imprimir expresion "afirmaciones")) "sust-num "))) "sust-alias"))]))

(define (evaluar-aux expresion)
  (cond [(not(equal? (obtener-reduccion-beta expresion) expresion))
         (if (equal? 1 (tamano expresion))
              expresion
             (evaluar-aux (imprimir (obtener-reduccion-beta expresion)"red beta")))]
        [else (cond [(equal? expresion TRUE) (imprimir 'TRUE "sust-formula")] 
               [(equal? expresion FALSE) (imprimir 'FALSE "sust-formula")]
               [(equal? expresion (formula-a-numero expresion)) (imprimir expresion "")]
               [else (imprimir (formula-a-numero expresion) "sust-formula")])])        
  )

(define (obtener-reduccion-beta expresion)
  (cond
    [(null? expresion) null]
    [(not(list? expresion)) (list expresion)]
    [(equal? 1 (tamano expresion))
     (cond
       [(null? (car expresion)) null]
       [(not(pair? (car expresion))) (car expresion)] 
       [(equal? 2 (tamano (car expresion)))  (obtener-reduccion-beta (car expresion))]
       [ else (unir (obtener-definicion (car expresion))
                      (list(obtener-reduccion-beta (obtener-cuerpo (car expresion))))) ])]
    [(equal? 2 (tamano expresion))
       (if (and(list? (car expresion)) (equal? 2 (tamano (car expresion))))
                 (cons (obtener-reduccion-beta (car expresion))
                       (list(obtener-reduccion-beta (cdr expresion))))
                (reduccion-beta (list (car expresion) (obtener-reduccion-beta (cdr expresion)))))]
    [else (unir (obtener-definicion expresion)
                  (list (obtener-reduccion-beta  (obtener-cuerpo expresion))))]))

(define (SETF expresion)
  (cond [(null? expresion) null]
        [(equal? (car expresion) 'TRUE) (cons TRUE (SETF (cdr expresion)))]
        [(equal? (car expresion) 'FALSE) (cons FALSE (SETF (cdr expresion)))]
        [(equal? (car expresion) 'plus) (cons plus (SETF (cdr expresion)))]
        [(equal? (car expresion) 'resta) (cons sub (SETF (cdr expresion)))]
        [(equal? (car expresion) 'mult) (cons mult (SETF (cdr expresion)))]
        [(equal? (car expresion) 'succ) (cons succ (SETF (cdr expresion))) ]
        [(equal? (car expresion) 'pred) (cons pred (SETF (cdr expresion))) ]
        [(equal? (car expresion) 'pow) (cons pow (SETF (cdr expresion))) ]
        [(equal? (car expresion) 'andd) (cons andd (SETF (cdr expresion))) ]
        [(equal? (car expresion) 'orr) (cons orr (SETF (cdr expresion))) ]
        [(equal? (car expresion) 'nott) (cons nott (SETF (cdr expresion))) ]
        [(equal? (car expresion) 'ifthenelse) (cons ifthenelse (SETF (cdr expresion))) ]
        [(equal? (car expresion) 'iszero) (cons iszero (SETF (cdr expresion))) ]
        [(list? (car expresion)) (cons (SETF (car expresion)) (SETF (cdr expresion)))]
        [else (cons (car expresion) (SETF (cdr expresion)))]))

(define (unir l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (cons (car l1) (unir (cdr l1) l2))]))

(define (obtener-definicion expresion)
  (cond [(null? expresion) null]
        [(not(list? expresion)) expresion]
        [(equal? '_ (car expresion) ) (cons '_ null)]
        [else (cons (car expresion) (obtener-definicion (cdr expresion)))]))

;; Funcionalidades
(define plus '(L m _ (L n _ (L f _ (L x _ ((m f) ((n f) x)) ) ) ) ))

(define ((PLUS x) y)
  (evaluar (list ( list plus x ) y)))

(define sub '( L m _ (L n _ ((n pred) m))))

(define ((SUB x) y)
  (if (> y x)
      (imprimir "no se puede realizar esta operacion" ".")
      ( evaluar (list (list sub x ) y))))

(define mult '(L m _ (L n _ (L f _ (m (n f))))))

(define ((MULT x) y)
  (evaluar  (list (list mult x) y)))

(define pow '(L b _ (L e _ ((e (L x _ ((mult b) x)) )1))) )

(define ((POW b)e)
  (evaluar (list (list pow b) e)))

(define succ '(L n _ (L f _ ( L x _ ( f ((n f) x))))))

(define (SUCC x)
  (evaluar (list succ x)))

(define pred '(L n _(L f _ (L x _ ( ( ( n (L g _ (L h _ (h (g f)))) ) (L u _ x)) (L u _ u))))))

(define (PRED a)
  (evaluar (list pred a)))

(define TRUE '(L x _ (L y _ x)))

(define FALSE '(L x _ (L y _ y)))

(define andd '(L p _ (L q _ ((p q) FALSE ))) )

(define ((AND x)y)
  (evaluar (list (list andd x) y)))

(define orr '(L p _ (L q _ ((p TRUE) q))) )

(define ((OR x) y)
  (evaluar (list (list orr x) y)))

(define nott '(L p _ ((p FALSE ) TRUE)) )

(define (NOT x)
  (evaluar (list nott x)))

(define ifthenelse '(L b _ (L x _ (L y _ ((b x) y)))))

(define (((IFTHENELSE cond) then)else)
  (evaluar (list (list (list '(L b _ (L x _ (L y _ ((b x) y)))) cond) then) else)))

(define iszero '(L n _ ( (n (L x _ (L x _ (L y _ y)))) (L x _ (L y _ x)))))

(define (ISZERO x)
  (evaluar (list iszero x)))

;; funcionalidad adicional

(define ((XOR x)y)
  (evaluar (list (list  '(L x _ (L y _ ((orr ((andd (nott x)) y)) ((andd x ) (nott y)))))  x) y)))

(define ((NAND x)y)
  (evaluar (list (list  '(L x _ (L y _ (nott ((andd x) y))))  x) y)))
;;funcion encargada de obtener datos del teclado
(define (leer )
	(read))

;; operaciones del criterio
;;Habilite y deshabilite segun su necesidad

;((POW 5)5)
;((SUB 7)4)
;(PRED 4)
;(ISZERO 5)
;(ISZERO 0)
;(((IFTHENELSE TRUE)'a)'b)
;(((IFTHENELSE FALSE)'a)'b)
;((MULT 0)5)
;((MULT 2)2)
;(ISZERO ((SUB 5)5))
;(((IFTHENELSE (ISZERO ((SUB 2)2)))((PLUS 2)2))((MULT 2)1))

;;Funciones de depuracion
;;Habilite y deshabilite segun su necesidad
(trace inicio)
;;(trace validar)
;;(trace sustituir)
;(trace evaluar)
;(trace imprimir)
(trace leer)