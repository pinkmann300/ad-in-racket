#lang typed/racket

(require math/flonum)

; Type classes : A set of operations which are valid over a range of types. For example - Eq type class consists of (==) and (/=). These functions
; can only take variables which have can be compared as equal or unequal.

; Instance declaration : Each instance declaration makes a specific type belong to the type class. For example, the type Int is an instance of the
; Eq class because you can check whether two integers are equal or not.

(struct (t) Num
  ([add : (-> t t t)]
   [mul : (-> t t t)]
   [neg : (-> t t)]
   [div : (-> t t t)]
   [sqrt : (-> t t)]
   [absol : (-> t t)]
   [signum : (-> t t)]
   [fromFloat : (-> Float t)])
  #:transparent)

; Little bit confused on whether to include an unequal function or not but realised that
;  unequal would basically be the negation of the equal function and hence decided not to.
; Carried forward the same decision with respect to the Ord type class as well. 

(struct (t) Eq
  ([equal : (-> t t Boolean)]))

(struct (t) Ord
  ([geq : (-> t t Boolean)]))

(struct Dual ([x : Float] [dx : Float])
  #:transparent)

(struct (t) Floating
  ([pi : (-> t)]
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
   [atanh : (-> t t)]))


(define DualFloat
  (Floating
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

; The below is an instance declaration of the Num type class where we accept Dual Numbers to be a valid member of the Num type class
; and define the operations that the Num class can perform on them.

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
   (λ ([a : Dual] [b : Dual])
     (Dual (* (Dual-x a) (Dual-x b))
           (+ (* (Dual-x a) (Dual-dx b))
              (* (Dual-x b) (Dual-dx a)))))
   (λ ([a : Dual])
     (Dual (flsqrt (Dual-x a))
           (* (/ 1.0 (* 2.0 (flsqrt (Dual-x a)))) (Dual-dx a))))
   (λ ([a : Dual])
     (Dual (abs (Dual-x a)) (* (Dual-dx a) (sgn (Dual-x a)))))
   (λ ([a : Dual])
     (Dual (sgn (Dual-x a)) 0.0))
   (λ ([x : Float])
     (Dual x 0.0))))

; The below is an example of another instance declaration where we make the DualNum an instance of the 
(define DualEq
  (Eq
   (λ ([a : Dual] [b : Dual])
     (eq? (Dual-x a) (Dual-x b)))))

(define DualOrd
  (Ord
   (λ ([a : Dual] [b : Dual])
     (<= (Dual-x a) (Dual-dx b)))))

(: square (All (t) (-> (Num t) t t)))
(define (square tc x)
  ((Num-mul tc) x x))

(: checkInEquality (All (t) (-> (Eq t) t t Boolean)))
(define (checkInEquality tc a b)
  (not ((Eq-equal tc) a b)))

(: x (-> Float Dual))
(define (x a)
  (Dual a 1.0))

