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

(struct (t) Num
  ([add : (-> t t t)])
  #:transparent)


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
       [(list (Zero) b) (Zero)]
       [(list a (Zero)) (Zero)]
       [(list a b) (Taylor (fl* (f a) (f b))
                           (TaylorAdd
                            (TaylorMul (df a) b)
                            (TaylorMul (df b) a)))]))


(: TaylorNegate (-> TaylorZ TaylorZ))
(define
  (TaylorNegate [a : TaylorZ])
     (match a
       [(Zero) (Zero)]
       [a  (Taylor (fl* -1.0 (f a))
                   (TaylorNegate (df a)))]))
#|
(: TaylorSignum (-> TaylorZ TaylorZ))
(define
  (TaylorSignum [a : TaylorZ])
     (match a
       [(Zero) (Zero)]
       [a (Taylor (signum 
|#

