#lang racket
(require (planet aml/rosetta))
(backend autocad)

;;;; CORPO EXTERIOR 

;;; PLANTA EXTERIOR

;; Define pontos da planta exterior
; p - coordenadas do ponto central
; rb - raio da base
; hf - altura final
; ai - incremento da ângulo inicial
; n-lados - número de lados da planta-exterior
(define (pontos-planta-exterior p rb hf ai n-lados)
  (map-division
   (lambda (fi)
     (+pol p (* rb (- 1 (/ (cz p) hf))) (+ ai fi)))
   0 2pi n-lados #t))

;;; CORPO EXTERIOR

(define (corpo-exterior p rb hf ai n-lados ptss)
  (map (lambda (p)
         (pontos-planta-exterior p rb hf ai n-lados))
       ptss))

;;; EIXO HELICOIDAL - Eixo Real

;; Define pontos do eixo em hélice
; ri - raio inicial da hélice
; rf - raio final da hélice
; af - incremento de ângulo final da hélice
; hi - altura inicial da hélice
; n-pisos - número de pisos
(define (pontos-eixo-helice p ri rf ai af hi hf n-pisos)        
  (map (curry +cyl p)
       (division ri rf n-pisos)
       (division ai af n-pisos)
       (division hi hf n-pisos)))

;; Aplicação da planta-exterior à hélice
(define (corpo-exterior-helicoidal p rb ri rf hi hf ai af n-lados n-pisos)
  (corpo-exterior p rb hf ai n-lados
           (pontos-eixo-helice p ri rf ai af hi hf n-pisos)))

;;; EIXO SINUSOIDAL - Eixo alternativo

(define (sinusoide a omega fi z)
  (* a (sin (+ (* omega z) fi))))

;; Define um eixo sinusoidal
(define (pontos-eixo-sinusoidal p a omega fi z n)
  (map-division
   (lambda (z)
     (+xz p (sinusoide a omega fi z) z))
   0 z n))

;; Aplicação da planta-exterior à sinusóide
(define (corpo-exterior-sinusoidal p rb hf ai n-lados a omega fi z n)
  (corpo-exterior p rb hf ai n-lados
           (pontos-eixo-sinusoidal p a omega fi z n)))

;;; PILARES

(define (matriz-transposta matriz)
  (if (null? (car matriz))
      (list)
      (cons (map car matriz)
            (matriz-transposta (map cdr matriz)))))

;; Desenha os pilares
(define (pilares p rb ri rf ai af hi hf n-lados n-pisos)
  (with-current-layer "Pilares-Aço"
                      (union
                       (map
                        (lambda (pts)
                          (sweep
                           (spline pts)
                           (surface-circle (u0) 0.4)))
                        (matriz-transposta (corpo-exterior-helicoidal p rb ri rf hi hf ai af n-lados n-pisos))))))


;;; VIDROS

(define (itera-quadrangulos f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (f p0 p1 p2 p3))))

;; Desenha os vidros
(define (vidros ptss)
  (with-current-layer "Vidros"
                      (union
                       (itera-quadrangulos
                        (lambda (p0 p1 p2 p3)
                          (surface-polygon p0 p1 p2 p3))
                        ptss))))
;;; VIGAS

(define (vigas ptss)
  (with-current-layer "Vigas-Aço"
                      (union
                       (itera-quadrangulos
                        (lambda (p0 p1 p2 p3)
                          (let ((r 0.2))
                            (cylinder p0 r p1)
                            (cylinder p2 r p3)))
                        ptss))))

;;; DIAGONAIS

;; Define uma matriz com listas de pontos de j em j pisos
; j - incremento entre pisos
(define (pontos-diagonais p rb ri rf ai af hi hf n-lados n-pisos j)
  (map (lambda (i)
         (list-ref (corpo-exterior-helicoidal p rb ri rf hi hf ai af n-lados n-pisos) i))
       (range 0 n-pisos j)))

;;Desenha as diagonais
(define (diagonais ptss)
  (with-current-layer "Diagonais-Aço"
                      (union
                       (itera-quadrangulos
                        (lambda (p0 p1 p2 p3)
                          (let ((r 0.4))
                            (cylinder p0 r p2)
                            (cylinder p1 r p3)))
                        ptss))))


;;;; CORPO INTERIOR

;;Define o corpo interior do edíficio
; p - coordenadas do vértice do corpo interior central
; c - comprimento do corpo interior
; l - largura do corpo interior
; h1 - altura do corpo interior central
; h2 - altura do corpo interior 2
; h3 - altura do corpo interior 3
; h4 - altura do corpo interior 4

(define (corpo-interior p c l h1 h2 h3 h4)
 (with-current-layer "CorpoInterior-Betão" 
  (union 
   (box p c l h1)
   (box (+x p (- c))
        c
        l
        h2)
   (box (+x p c)
        c
        l
        h3)
   (box (+y p l)
        c
        l
        h4))))


;;;; TORRE DUBAI

(define (torre-dubai p rb ri rf ai af hi hf n-lados n-pisos j c l h1 h2 h3 h4)
  (pilares p rb ri rf ai af hi hf n-lados n-pisos)
  (vidros (matriz-transposta (corpo-exterior-helicoidal p rb ri rf hi hf ai af n-lados n-pisos)))
  (vigas (matriz-transposta (corpo-exterior-helicoidal p rb ri rf hi hf ai af n-lados n-pisos)))
  (diagonais (matriz-transposta (pontos-diagonais p rb ri rf ai af hi hf n-lados n-pisos j)))
  (corpo-interior p c l h1 h2 h3 h4))

(torre-dubai (+pol (u0) 200 pi) 40 0 40 pi 2pi 0 400 8 100 10 5 4 150 100 50 50)
(torre-dubai (+pol (u0) 200 (* 5 pi/4)) 50 0 40 0 pi 0 550 8 138 10 5 4 240 200 100 50)
(torre-dubai (+pol (u0) 200 (* 7 pi/4)) 38 0 40 pi/2 (* 3 pi/2) 0 480 8 120 10 5 4 200 150 100 50)
(torre-dubai (+pol (u0) 200 0) 36 0 40 2pi 3pi 0 360 8 90 10 5 4 100 90 50 40)

;;;; ILHA

(define (superelipse p a b n t)
  (+xy p 
       (* a 
          (expt (sqr (cos t)) (/ 1 n))
          (sgn (cos t)))
       (* b
          (expt (sqr (sin t)) (/ 1 n))
          (sgn (sin t)))))

;; Define pontos e desenha a superelipse
(define (curva-superelipse p a b n n-pontos)
  (closed-spline
   (map-division
    (lambda (t)
      (superelipse p a b n t))
      -pi
      pi
      n-pontos #f)))

;; Desenha a ilha
; rc - raio da base do cilindro
(define (ilha p a b n n-pontos rc)
  (with-current-layer "Ilha-Betão"
                      (subtraction
                       (cylinder p rc 5)
                       (extrusion 
                        (surface 
                         (curva-superelipse 
                          (+pol p rc pi/2) 
                          a b n n-pontos)) 
                        10))))

(ilha (xyz 0 0 -5) 80 150 2 4 400)


;;;; MAR
; r - raio do superfície curva
(define (mar p r)
  (with-current-layer "Mar-Água"
                      (surface-circle
                       (+z p -5) r )))
                     
(mar (u0) 10000)
