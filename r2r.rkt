#lang racket

(require math/base)
(require math/flonum)

; The below is forward mode automatic differentiation. 

;Duplicate function
(define (dup a)
  (cons a a))

;Extract right
(define (exr a)
  (match a
    [(cons k m) m]))

; Negate function for the vibes

(define negate
  (λ (k)
    (- k)))

;Extract left 
(define (exl a)
  (match a
    [(cons k m) k]))

; Uncurried addition
(define (un-add m)
  ((λ
      (k)
    (+ (exl k)
       (exr k))) m))

; Uncurried multiplication
(define (un-mul m)
    ((λ
      (k)
    (* (exl k)
       (exr k)))
     m))

; Scalar multiplication
(define scale
  (λ (k)
    (λ (m)
      (* k m))))

; Cross functions
(define cross
  (λ (k m)
    (λ (v)
      (cons (k (exl v)) (m (exr v))))))

; Sequential composition
(define fun-comp
  (λ (k m)
    (λ (v)
      (k (m v)))))

; Square function 
(define (sqr)
  (fun-comp un-mul dup))

; delta function
(define (delta f g)
  (fun-comp (cross f g) dup))

; downTri function
(define (downtri f g)
  (fun-comp un-add (cross f g)))

; linearCase function
(define (linF f a)
  (cons (f a) f))

; linearCase for identity function
(define dId
  (λ (k)
    (linF identity k)))

;linearCase for duplicate function
(define dDup
  (λ (k)
    (linF dup k)))

;linearCase for fst, snd and add since these are linear functions. 

;Defined it seperately to enforce the const characteristic observed in 
(define zero 0)

(define dFst
  (λ (k)
    (linF exl k)))

(define dSnd
  (λ (k)
    (linF exr k)))

(define dAdd
  (λ (k)
    (linF un-add k)))

(define dScale
  (fun-comp linF scale))

; Derivatives of constants will always be zero
(define (dConst n)
  (cons n zero))


; Parallel composition of differentiable functions 
(define (dCross f g)
  (λ (k)
    (let ([m ((cross f g) k)])
      (cons (cons (car (car m)) (car (cdr m)))
            (cross (cdr (car m)) (cdr (cdr m)))))))

; Sequential composition of differentiable functions
(define (dComp g f)
  (λ (k)
    (let* ([x (f k)]
           [y (g (car x))])
      (cons (car y) (fun-comp (cdr y) (cdr x))))))

; Product rule included
(define (dMul k)
  (cons (* (car k) (cdr k)) (downtri (scale (cdr k)) (scale (car k)))))

; Differentiable square function
(define dSqr
  (dComp dMul dDup))

; Working and tested with sample values as well
; Non linear primitives will be defined from here on

; Main product rule 
(define (prodRu f1 f2)
  (delta f1 (fun-comp scale f2)))

(define dExp
  (prodRu exp exp))

(define dLog
  (prodRu log (λ (k) (/ 1 k))))

(define dSin
  (prodRu sin cos))

(define dCos
  (prodRu cos (fun-comp (λ (k) (- k)) sin)))

(define dAsin
  (prodRu asin (λ (x)
                 (/ 1 (sqrt (- 1 (* x x)))))))

(define dAcos
  (prodRu acos (λ (x)
                 (* -1 (/ 1 (sqrt (-1 (* x x))))))))

(define dAtan
  (prodRu atan (λ (x)
                 (/ 1 (+ (* x x) 1)))))

(define dSinh
  (prodRu sinh cosh))

(define dCosh
  (prodRu cosh sinh))

(define dAsinh
  (prodRu asinh (λ (x)
                  (/ 1 (sqrt (+ (* x x) 1))))))

(define dAcosh
  (prodRu acosh (λ (x)
                  (negate (/ 1 (sqrt (- (* x x) 1)))))))

(define dAtanh
  (prodRu atanh (λ (x)
                  (/ 1 (- 1 (* x x))))))

(define (dPow a b)
  (cons (expt a b)
        (downtri
         (scale (* b (expt a (- b 1))))
         (scale (* (log a) (expt a b))))))



