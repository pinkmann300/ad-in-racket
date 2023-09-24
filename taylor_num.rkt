#lang typed/racket

(require typed/test-engine/racket-tests)
(require math/flonum)
(require racket/flonum)

; My Haskell Saviour:
; data Taylor x = Zero | Taylor x (Taylor x)

(: factorial (-> Integer Integer))
(define (factorial n)
  (if (eq? n 0)
      1
      (* n (factorial (- n 1)))))

(define-type TaylorZ (U Zero Taylor))

(struct Zero ()
  #:transparent)

(struct Taylor
  ([x : Flonum] [dx : TaylorZ])
  #:transparent)

(: f (-> TaylorZ Flonum))
(define (f t)
  (if (eq? (Zero) t)
      0.0
      (if (Taylor? t)
          (Taylor-x t)
          0.0)))
     
(check-within (f (Zero)) 0.0 0.000000001)
(check-within (f (Taylor 2.0 (Taylor 5.0 (Zero)))) 2.0 0.0000001)

(: df (-> TaylorZ TaylorZ))
(define (df t)
  (if (eq? t (Zero))
      (Zero)
      (if (Taylor? t)
          (Taylor-dx t)
          (Zero))))

;(check-expect (df (Taylor 2.0 (Taylor 5.0 (Zero)))) (Taylor 5.0 (Zero)))

(: dfn (-> Integer (-> TaylorZ Flonum)))
(define (dfn n)
  (if (eq? n 0)
      f
      (lambda (x)
              ((compose (dfn (- n 1)) df) x))))
      
(check-within ((dfn 0) (Taylor 5.0 (Taylor 2.0 (Zero)))) 5.0 0.0001)

(: x (-> Flonum TaylorZ))
(define
  (x a)
  (Taylor a 1.0))


(: teval (-> Integer TaylorZ Flonum Flonum))
(define (teval k t x)
  (match (list k t x)
    [(list 0 t x) (f t)]
    [(list k (Zero) x) 0.0]
    [(list k t x)
     (fl+
      (teval (- k 1) t x)
      (fl/
       (fl* ((dfn k) t) (flexpt x (->fl k)))
       (->fl (factorial k))))]))


(: TaylorAdd (-> TaylorZ TaylorZ TaylorZ))
(define
  (TaylorAdd [a : TaylorZ] [b : TaylorZ])
     (match (list a b)
       [(list (Zero) b) b]
       [(list a (Zero)) a]
       [(list a b) (Taylor (fl+ (f a) (f b))
                           (TaylorAdd (df a) (df b)))]))

(: TaylorMul (-> TaylorZ TaylorZ  TaylorZ))
(define
  (TaylorMul [a : TaylorZ] [b : TaylorZ])
     (match (list a b)
       [(list (Zero) b) 0.0]
       [(list a (Zero)) 0.0]
       [(list a b) (Taylor (fl* (f a) (f b))
                           (TaylorAdd
                            (TaylorMul (df a) b)
                            (TaylorMul (df b) a)))])

(: TaylorNegate (-> TaylorZ TaylorZ))
(define
  (TaylorNegate [a : TaylorZ])
     (match a
       [(Zero) 0.0]
       [a  (Taylor (fl* -1.0 (f a))
                   (TaylorNegate (df a)))]))

(: TaylorSignum (-> TaylorZ TaylorZ))
(define
  (TaylorSignum [a : TaylorZ])
     (match a
       [(Zero) 0.0]
       [a (Taylor (sgn (f a))
                  (TaylorSignum (df a)))]))

(: TaylorAbsol (-> TaylorZ TaylorZ))
(define
  (TaylorAbsol [a : TaylorZ])
     (match a
       [(Zero) 0.0]
       [a (Taylor (sgn (f a))
                  0.0)]))

(: TaylorFromFloat (-> Flonum TaylorZ))
(define
  (TaylorFromFloat [a : Flonum])
     (match a
       [0.0 (Zero)]
       [a (Taylor a 0.0)]))


(struct (t) Num
  ([add : (-> t t t)]
   [mul : (-> t t t)]
   [neg : (-> t t)]
   [absol : (-> t t)]
   [signum : (-> t t)]
   [fromFloat : (-> Float t)])
  #:transparent)


; Instance declaration for the TaylorNum to be a part of the Num typeclass

(define TaylorNum
  (Num
   (λ ([a : TaylorZ] [b : TaylorZ])
     (TaylorAdd a b))
   (λ ([a : TaylorZ] [b : TaylorZ])
     (TaylorMul a b))
   (λ ([a : TaylorZ])
     (TaylorNegate a))
   (λ ([a : TaylorZ])
     (TaylorAbsol a))
   (λ ([a : TaylorZ])
     (TaylorSignum a))
   (λ ([a : Flonum])
     (TaylorFromFloat a))))

(struct (t) Eq
  ([equal : (-> t t Boolean)])
  #:transparent)

(define TaylorEq
  (Eq
   (λ ([a : TaylorZ][b : TaylorZ])
     (eq? (f a) (f b)))))

(struct (t) Ord
  ([geq : (-> t t Boolean)])
  #:transparent)

(define TaylorOrd
  (Ord
   (λ ([a : TaylorZ][b : TaylorZ])
     (<= (f a) (f b)))))

;; Having some trouble introducing the fractional type because in haskell Fractional is a part of the Num class if im not wrong


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


             
                