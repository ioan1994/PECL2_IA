#lang racket
(define longitudX '(841 594 420 297 210 148 105 74 52 37 26))
(define longitudY '(1189 841 594 420 297 210 148 105 74 52 37))
(define nodoInicial '(841 1189))

;Devuelve el número de cortes que hay que realizar en las dimensiones en el papel para conseguir una determinada longitud.
;Prerequisito: la longitud a la que se quiere llegar tiene que 
(define numeroCortes (lambda (cortes dimensiones longitud) (
           if (= (first dimensiones) longitud)
              (cortes)
              (numeroCortes (+ 1 cortes) (rest dimensiones) longitud)
           )))

;Comprueba si es solucion pasandole el número de cortes y el tamaño x e y del folio actual.
(define esSolucion (lambda (cortes dimension) (
       and (= (numeroCortes 0 longitudX (first dimension)) cortes) (= (numeroCortes 0 longitudY (second dimension)) cortes)                              
       )))

(define alfabeta (lambda (cortes nodo alfa beta turnoJugador) (
        if(esSolucion cortes nodo)         
        (
          ;quitar
          #f
        )
        ;else
        (
          ;quitar
          #f
        )
        )))
  ;métdo principal que sortea quien empieza 
(define jugar (lambda (cortes) (
     if (= (random 2) 0) (print "Empieza el ordenador") (print "Empieza el jugador")                           
     )))