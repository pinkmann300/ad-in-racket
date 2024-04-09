#lang typed/racket

(require math/flonum)

(struct (t) Num
  ([add : (-> t t t)]
   [mul : (-> t t t)]
   [neg : (-> t t)]
   [abso : (-> t t)]
   [signum : (-> t t)]
   [fromInt : (-> Float t)]
   [sqrt : (-> t t)]
   [pi : (-> t)]
   [exp : (-> t t)]
   [log : (-> t t)]
   [sin : (-> t t)]
   [cos : (-> t t)]
   [arcsin : (-> t t)]
   [arccos : (-> t t)]
   [arctan : (-> t t)]
   [sinh : (-> t t)]
   [cosh : (-> t t)]
   [asinh : (-> t t)]
   [acosh : (-> t t)]
   [atanh : (-> t t)])
  #:transparent)

(struct Dual ([x : Float] [dx : Float])
  #:transparent)

(define DualNum
  (Num
   (λ ([a : Dual] [b : Dual])
     (Dual (+ (Dual-x a) (Dual-x b))
           (+ (Dual-dx a) (Dual-dx b))))
   (λ ([a : Dual] [b : Dual])
     (Dual (* (Dual-x a) (Dual-x b))
           (+ (* (Dual-x a) (Dual-dx b))
              (* (Dual-x b) (Dual-dx a)))))
   (λ ([a : Dual])
     (Dual (- (Dual-x a)) (- (Dual-dx a))))
   (λ ([a : Dual])
     (Dual (sgn (Dual-x a)) 0.0))
   (λ ([a : Dual])
     (Dual (abs (Dual-x a)) (* (Dual-dx a) (sgn (Dual-x a)))))
   (λ ([x : Float])
     (Dual x 0.0))
   (λ ([a : Dual])
     (Dual (flsqrt (Dual-x a))
           (* (/ 1.0 (* 2.0 (flsqrt (Dual-x a)))) (Dual-dx a))))
   (λ ()
     (Dual pi 0.0))
   (λ ([a : Dual])
     (Dual (exp (Dual-x a)) (* (Dual-dx a) (exp (Dual-x a)))))
   (λ ([a : Dual])
     (Dual (fllogb 10.0 (Dual-x a)) (/ (Dual-dx a) (Dual-x a))))
   (λ ([a : Dual])
     (Dual (sin (Dual-x a)) (* (Dual-dx a) (cos (Dual-x a)))))
   (λ ([a : Dual])
     (Dual (cos (Dual-x a)) (* (* -1 (Dual-dx a)) (sin (Dual-x a)))))
   (λ ([a : Dual])
     (Dual (asin (Dual-x a)) (/ (Dual-dx a) (flsqrt (- 1 (* (Dual-x a) (Dual-x a)))))))
   (λ ([a : Dual])
     (Dual (acos (Dual-x a)) (/ (* -1 (Dual-dx a)) (flsqrt (- 1 (* (Dual-x a) (Dual-x a)))))))
   (λ ([a : Dual])
     (Dual (atan (Dual-x a)) (/ (Dual-dx a) (+ 1 (* (Dual-x a) (Dual-x a))))))
   (λ ([a : Dual])
     (Dual (flsinh (Dual-x a)) (* (Dual-dx a) (cosh (Dual-x a)))))
   (λ ([a : Dual])
     (Dual (flcosh (Dual-x a)) (* (Dual-dx a) (sinh (Dual-x a)))))
   (λ ([a : Dual])
     (Dual (flasinh (Dual-x a)) (/ (Dual-dx a) (flsqrt (+ 1 (* (Dual-x a) (Dual-x a)))))))
   (λ ([a : Dual])
     (Dual (flacosh (Dual-x a)) (/ (Dual-dx a) (flsqrt (- (* (Dual-x a) (Dual-x a)) 1)))))
   (λ ([a : Dual])
     (Dual (flatanh (Dual-x a)) (/ (Dual-dx a) (+ 1 (* (Dual-x a ) (Dual-x a))))))))


(: constD (-> Float Dual))
(define (constD n)
  (Dual n 1.0))

(: square (All (t) (-> (Num t) t t)))
(define (square tc x)
  ((Num-mul tc) x x))

(: cube (All (t) (-> (Num t) t t)))
(define (cube tc x)
  ((Num-mul tc) x ((Num-mul tc) x x)))

; Test case:

(: k Dual)
(define k (square DualNum (Dual 5.0 1.0)))
(define q (cube DualNum (Dual 5.0 1.0)))




