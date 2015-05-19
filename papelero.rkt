#lang racket
(define longitudX '(841 594 420 297 210 148 105 74 52 37 26))
(define longitudY '(1189 841 594 420 297 210 148 105 74 52 37))
(define nodoInicial '(841 1189))

(define true? 
  (lambda (verdad)
    (
     if(false? verdad) #f #t)))

(define numeroCortes 
  (lambda (cortes dimensiones longitud) 
    (
     if(empty? dimensiones) 0
       (
        if (equal? (first dimensiones) longitud)
           cortes
           (numeroCortes (+ 1 cortes) (rest dimensiones) longitud)
           )
       )))

(define getcortesY 
  (lambda (nodo cortes n)
    (
     if(equal? n 0) '()
       (
        if(equal? (second nodo) (first cortes))
          (take (rest cortes) n)
          (getcortesY nodo (rest cortes) (- n 1))
          )
       )))

(define getcortesX 
  (lambda (nodo cortes n)
    (
     if(equal? n 0) '()
       (
        if(equal? (first nodo) (first cortes))
          (take (rest cortes) n)
          (getcortesX nodo (rest cortes) (- n 1))
          )
       )))

(define construirSucesoresY
  (lambda (x sucesores)
    (
     if(empty? sucesores) '()
       (list* (list x (first sucesores)) (construirSucesoresY x (rest sucesores)))
       )))
       
(define construirSucesoresX
  (lambda (y sucesores)
    (
     if(empty? sucesores) '()
       (list* (list (first sucesores) y) (construirSucesoresX y (rest sucesores)))
       )))
       
(define getSucesores
  (lambda (nodo n)
    (
     append (construirSucesoresY (first nodo) (getcortesY nodo longitudY n)) (construirSucesoresX (second nodo) (getcortesX nodo longitudX n))
            )))


(define solucion? 
  (lambda (cortes dimension)
    (
     and (equal? (numeroCortes 0 longitudX (first dimension)) cortes) (= (numeroCortes 0 longitudY (second dimension)) cortes)                              
         )))

(define calcularNodo 
  (lambda (n nodo sucesores maquina)
    (
     if(solucion? n nodo)
       (
        if(true? maquina) 0 1
          )
       (
        if(true? maquina)
          (
           if(empty? (rest sucesores)) (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina))
             (
              max (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina)) (calcularNodo n nodo (rest sucesores) maquina)  
                  )
             )
          (
           if(empty? (rest sucesores)) (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina))
             (
              min (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina)) (calcularNodo n nodo (rest sucesores) maquina) 
                  )
             )
          )
       )))

(define minimax 
  (lambda (n nodo sucesores)
    (
     if(empty? (rest sucesores)) (first sucesores)
       (
        if(= (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) #t) 1) 
          (first sucesores)
          (minimax n nodo (rest sucesores))
          )
       )))


(define leerNumero 
  (lambda (numero sucesores)
    (
     if(number? numero)
       (
        if(<= numero (length sucesores))
          (
           if(= numero 0) (first sucesores)
             (leerNumero (- numero 1) (rest sucesores))
             )
          (
           leerNumero (read) sucesores (display "El número se excede de los límites, vuelve a insertarlo...")
                      )
          )
       (
        (display "El caracter introducido no es válido, vuelve a hacerlo...") 
        (leerNumero (read) sucesores)                                                                      
        )
       )))

(define main 
  (lambda (n maquina nodo)
    (
     if(solucion? n nodo) 
       (
        if(true? maquina) 
          (
           display "Has ganado!"
                   ) 
          (
           display "Ha ganado la máquina"
                   )
          )
       (
        if(true? maquina)
          ((lambda(x) 
             ((lambda(x)  
                ((lambda(x) 
                   ((lambda(x)  
                      ((lambda(x) 
                         ((lambda(x) 
                            ((lambda(x) 
                               ((lambda(x) 
                                  ((lambda(x) 
                                     ((lambda(x) 
                                        (main n (not maquina) (minimax n nodo (getSucesores nodo n))))
                                      (newline))) 
                                   (display (minimax n nodo (getSucesores nodo n)))))
                                (display "El ordenador ha escojido cortar hasta tener ")))
                             (newline)))
                          (display (getSucesores nodo n)) ))    
                       (display "El ordenador puede escojer")))
                    (newline)))
                 (display "Turno del ordenador")))
              (newline)))
           (display "----------------------------------------------------------"))
          
          ((lambda(x) 
             ((lambda(x) 
                ((lambda(x) 
                   ((lambda (x) 
                      ((lambda (x) 
                         ((lambda (x)
                            ((lambda (x)
                               ((lambda (x)
                                  ((lambda (x)
                                     (main n (not maquina) (leerNumero x (getSucesores nodo n))))
                                   (read)))
                                (newline)))
                             (display (getSucesores nodo n))))
                          (newline))) 
                       (display "Escoger entre el 0 y el máximo de los siguientes sucesores"))) 
                    (newline))) 
                 (display "Tu turno")))
              (newline))) 
           (display "----------------------------------------------------------"))
          )
       )))

(define jugar 
  (lambda (n) 
    (
     if (= (random 2) 0) (main n #f nodoInicial) (main n #t nodoInicial)                              
        )))
