#lang racket
(require math/base)

; The below is an implementation of an Automatic Differentiation system
; without any type enforcements. The code below has taken inspiration from
; https://crypto.stanford.edu/~blynn/haskell/ad.html

; The below system works only for systems of type R -> R and does not
; extend beyond that.

; We begin by defining a structure to store the function value and the
; value of the derivative itself.

; Some inbuilt Racket functions we will be using through the implementation
; will be the compose function which will be used for function composition
; and identity will be the id function from Prelude on Haskell which returns
; the same element that you pass as an argument to that function.

(struct tuple (fx fdx)
  #:transparent)

; fx denotes the value of the function at point x and
; fdx denotes the linear map that represents the derivative of f at x.

; Below we define the functions expressed in the Cartesian
; Category in our Haskell implementation present at
; https://github.com/pinkmann300/ad-in-racket/blob/main/conal.hs
(define (exl k)
  (match k
    [(cons x y) x]))

(define (exr k)
  (match k
    [(cons x y) y]))

(define (dup n)
  (cons n n))

(define uncurr-add
  (λ (k)
    (match k
      [(cons x y) (+ x y)])))

(define uncurr-mul
  (λ (k)
    (match k
      [(cons x y) (* x y)])))

; Will define the scale function to only be multiplication 
; since we are handling cases of R -> R functions
(define scale
         (λ (k)
           (λ (n)
             (* k n))))

(define (cross f g)
  (λ (k)
    (match k
      [(cons x y) (cons (f x) (g y))])))

; We now define the D instances for the functions.
(define (linearD f)
  (λ (a)
    (cons (f a) f)))

(define dIdentity
  (linearD identity))

(define dDup
  (linearD dup))

(define sqr
  (compose uncurr-mul dup))

(define dExl
  (linearD exl))

(define dExr
  (linearD exr))

; Linear functions for numerical operations.
(define dAdd
  (linearD uncurr-add))

(define (dConst n k)
  (cons n 0))

(define (tri f g)
  (compose (cross f g) dup))

(define (dtri f g)
  (compose (cross f g) dup))

(define (downtri f g)
  (compose uncurr-add (cross f g)))

(define (dDowntri f g)
  (dComp dAdd (dCross f g)))

; The Monoidal category instance for differentiable functions
; in the Haskell implementation will be taken 
; care of by the function dCross

(define (dCross f g)
  (λ (k)
  (let* ([result ((cross f g) k)])
    (let
        ([c (car (car result))]
         [d (car (cdr result))]
         [fdash (cdr (car result))]
         [gdash (cdr (cdr result))])
      (cons (cons c d) (cross fdash gdash))))))

; The Category composition instance for differentiable 
; functions will be taken care of by the dComp function.

(define (dComp g f)
  (λ (k)
    (let* ([fa (f k)]
           [gb (g fa)])
      (cons
       (car gb)
       (compose (cdr gb) (cdr fa))))))

;Unlike addition and negation, the differentiation rule is
;a little more involved for multiplication when it comes to
;linear maps.

(define (negate k)
  (- k))

(define dMul
  (λ (k)
    (match k
      [(cons (cons x y) f) (cons (* x y) (downtri (scale y) (scale x)))])))

(define (prodRu f fdash)
  (tri f (compose scale fdash)))

(define dExp
  (prodRu exp exp))

(define dLog
  (prodRu log (λ (k) (/ 1 k))))

(define dSin
  (prodRu sin cos))

(define dCos
  (prodRu cos (compose negate sin)))

(define dAsin
  (prodRu asin
          (λ
              (k)
            (/ 1 (sqrt (- 1 (* k k)))))))

(define dAcos
  (prodRu acos
          (λ
              (k)
            (negate
             (/ 1
                (sqrt
                 (- 1 (* k k))))))))

(define dAtan
  (prodRu atan
          (λ
              (k)
            (/ 1
               (+ 1 (* k k))))))

(define dSinh
  (prodRu sinh cosh))

(define dCosh
  (prodRu cosh sinh))

(define dAsinh
  (prodRu asinh
          (λ (k)
            (/ 1
               (sqrt (+ 1 (* k k)))))))

(define dAcosh
  (prodRu acosh
          (λ (k)
            (negate
             (/ 1
                (sqrt (- (* k k) 1)))))))

(define dAtanh
  (prodRu atanh
          (λ (k)
            (/ 1
               (- 1 (* k k))))))

(define (dPow k)
  (let ([a (tuple-fx k)]
        [b (tuple-fdx k)])
    (tuple (expt a b)
           (downtri
            (scale (* b (expt a (- b 1))))
            (scale (* (log a) (expt a b)))))))

(define dSqr2
  (dComp dMul dDup))
