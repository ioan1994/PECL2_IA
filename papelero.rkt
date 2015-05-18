#lang racket
(define longitudX '(841 594 420 297 210 148 105 74 52 37 26))
(define longitudY '(1189 841 594 420 297 210 148 105 74 52 37))
(define nodoInicial '(841 1189))

(define true? (lambda (verdad)(
           if(false? verdad) #f #t)))

(define numeroCortes (lambda (cortes dimensiones longitud) (
           if(empty? dimensiones) 0
           (
           if (equal? (first dimensiones) longitud)
              cortes
              (numeroCortes (+ 1 cortes) (rest dimensiones) longitud)
           )
           )))

(define getcortesY (lambda (nodo cortes n)(
       if(equal? n 0) '()
          (
          if(equal? (second nodo) (first cortes))
          (take (rest cortes) n)
          (getcortesY nodo (rest cortes) (- n 1))
          )
       )))

(define getcortesX (lambda (nodo cortes n)(
       if(equal? n 0) '()
          (
          if(equal? (first nodo) (first cortes))
          (take (rest cortes) n)
          (getcortesX nodo (rest cortes) (- n 1))
          )
       )))
;(construirSucesoresY 841 longitudY)
(define construirSucesoresY(lambda (x sucesores)(
         if(empty? sucesores) '()
         (list* (list x (first sucesores)) (construirSucesoresY x (rest sucesores)))
         )))
(define construirSucesoresX(lambda (y sucesores)(
         if(empty? sucesores) '()
         (list* (list (first sucesores) y) (construirSucesoresX y (rest sucesores)))
         )))
(define getSucesores(lambda (nodo n)(
         append (construirSucesoresY (first nodo) (getcortesY nodo longitudY n)) (construirSucesoresX (second nodo) (getcortesX nodo longitudX n))
         )))


(define solucion? (lambda (cortes dimension) (
       and (equal? (numeroCortes 0 longitudX (first dimension)) cortes) (= (numeroCortes 0 longitudY (second dimension)) cortes)                              
       )))

(define calcularNodo (lambda (n nodo sucesores maquina)(
       if(solucion? n nodo)
       (
         if(true? maquina) 1 0
       )
       (
         if(true? maquina)
         (
           if(empty? (rest sucesores)) (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina))
           (
             max (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina)) (calcularNodo n nodo (rest sucesores) (not maquina))  
           )
         )
         (
           if(empty? (rest sucesores)) (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina))
           (
             min (calcularNodo n (first sucesores) (getSucesores (first sucesores) n) (not maquina)) (calcularNodo n nodo (rest sucesores) (not maquina)) 
           )
         )
       )
       )))

(define jugar (lambda (n nodo sucesores maquina)(
         if(solucion? nodo) #t #t
         )))
