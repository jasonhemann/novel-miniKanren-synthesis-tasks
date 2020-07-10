#lang racket
(require (except-in rackunit fail) "mk.rkt" "fo-lavo.rkt")

(test-equal? "Query for a Quine"
 (run 1 (q) (evalo q q))
'((((λ (_0) (list _0 (list 'quote _0)))
   '(λ (_0) (list _0 (list 'quote _0))))
  (=/= ((_0 closure)) ((_0 list)) ((_0 quote)))
  (sym _0))))

(define-relation (all-diffo l)
  (conde
    ((== l '()))
    ((fresh (a d)
       (== l `(,a . ,d))
       (not-ino a d)
       (all-diffo d)))))

(define (not-ino x l)
  (conde
    ((== l '()))
    ((fresh (a d)
       (== l `(,a . ,d))
       (=/= x a)
       (not-ino x d)))))

(test-equal? "Oscillating Quines with Initial Transients"
 (run 1 (v t u q p) 
   (all-diffo '(v t u q p))
   (evalo q p) (evalo p q) (evalo u q) (evalo t u) (evalo v t))
'(((''''((λ (_0) (list 'quote (list _0 (list 'quote _0))))
        '(λ (_0) (list 'quote (list _0 (list 'quote _0)))))
   '''((λ (_0) (list 'quote (list _0 (list 'quote _0))))
       '(λ (_0) (list 'quote (list _0 (list 'quote _0)))))
   ''((λ (_0) (list 'quote (list _0 (list 'quote _0))))
      '(λ (_0) (list 'quote (list _0 (list 'quote _0)))))
   '((λ (_0) (list 'quote (list _0 (list 'quote _0))))
     '(λ (_0) (list 'quote (list _0 (list 'quote _0)))))
   ((λ (_0) (list 'quote (list _0 (list 'quote _0))))
    '(λ (_0) (list 'quote (list _0 (list 'quote _0))))))
  (=/= ((_0 closure)) ((_0 list)) ((_0 quote)))
  (sym _0))))

(test-equal? "Mirrored Programs"
  (run 3 (q) (fresh (a b) (evalo q a) (laveo q b)))
  '('etouq
    ((λ (_0) adbmal) (sym _0))
    ((λ (adbmal) adbmal) (λ (λ) adbmal))))

(test-equal? "2-cycle quine relay"
 (run 1 (q) (fresh (p) (laveo p q) (evalo q p)))
'((((λ (_0) (list (list _0 (list 'quote _0)) 'etouq))
    '(λ (_0) (list (list _0 (list 'quote _0)) 'etouq)))
   (=/= ((_0 closure)) ((_0 list)) ((_0 quote)))
   (sym _0))))

(test-equal? "A second 2-cycle quine relay"
 (run 1 (q) (fresh (p) (evalo q p) (laveo p q)))
'(('((('etouq
       (_0
        (((_0 (etouq etouq) tsil) (_1 etouq) tsil)
         ((_1) etouq)
         (adbmal etouq)
         tsil)
        tsil)
       tsil)
      (_0)
      adbmal)
     ((((('etouq
          (_0
           (((_0 (etouq etouq) tsil) (_1 etouq) tsil)
            ((_1) etouq)
            (adbmal etouq)
            tsil)
           tsil)
          tsil)
         (_0)
         adbmal)
        etouq)
       _1)
      (_1)
      adbmal))
   (=/= ((_0 closure)) ((_0 etouq)) ((_1 closure)) ((_1 etouq)))
   (sym _0 _1))))

(test-equal? "Boostrapping quines from ``damaged'' source"
  (run 3 (s r) 
    (fresh (p)
      (absento p s)
      (evalo `(,p ,r) `(,p ,r))
      (evalo `(,s ,r) `(,p ,r))))
  '((((λ (_0) (list _0 (list 'quote _0))) '(λ (_1) (list _1 (list 'quote _1))))
     (=/=
      ((_0 _1))
      ((_0 closure))
      ((_0 list))
      ((_0 quote))
      ((_1 closure))
      ((_1 list))
      ((_1 quote)))
     (sym _0 _1))
    ((((λ (_0) (λ (_1) (list _1 (list 'quote _1)))) '_2)
      '(λ (_3) (list _3 (list 'quote _3))))
     (=/=
      ((_0 list))
      ((_0 quote))
      ((_0 λ))
      ((_1 _3))
      ((_1 closure))
      ((_1 list))
      ((_1 quote))
      ((_3 closure))
      ((_3 list))
      ((_3 quote)))
     (sym _0 _1 _3)
     (absento ((λ (_3) (list _3 (list 'quote _3))) _2) (closure _2)))
    (((λ (_0)
        (list (list 'λ '(_1) '(list _1 (list 'quote _1))) (list 'quote _0)))
      '(λ (_1) (list _1 (list 'quote _1))))
     (=/=
      ((_0 closure))
      ((_0 list))
      ((_0 quote))
      ((_1 closure))
      ((_1 list))
      ((_1 quote)))
     (sym _0 _1))))

(test-equal? "Trivial and non-trivial ``stuttered'' quasi-quine"
  (run 2 (q) (evalo `(,q ,q) q))
'(quote
  (((λ (_0) (λ (_1) (list _0 (list 'quote _0))))
    '(λ (_0) (λ (_1) (list _0 (list 'quote _0)))))
   (=/=
    ((_0 _1))
    ((_0 closure))
    ((_0 list))
    ((_0 quote))
    ((_0 λ))
    ((_1 closure))
    ((_1 list))
    ((_1 quote)))
   (sym _0 _1))))

(test-equal? "3-cycle of distinct ``stuttered'' quasi-quines"
  (run 1 (a b c) 
    (fresh (t)
      (== t `((,a ,b) (,b ,c) (,c ,a)))
      (=/= a b) (=/= b c) (=/= c a)
      (evalo `(,a ,b) c)
      (evalo `(,b ,c) a)
      (evalo `(,c ,a) b)))
'((((λ (_0)
      '(λ (_1)
         (list
          'λ
          '(_2)
          (list
           'quote
           (list 'λ '(_0) (list 'quote (_1 '_3)))))))
    (λ (_2)
      '(λ (_0)
         '(λ (_1)
            (list
             'λ
             '(_2)
             (list
              'quote
              (list 'λ '(_0) (list 'quote (_1 '_3))))))))
    (λ (_1)
      (list
       'λ
       '(_2)
       (list 'quote (list 'λ '(_0) (list 'quote (_1 '_3)))))))
   (=/= ((_0 closure)) ((_0 quote)) ((_1 closure)) ((_1 list)) 
        ((_1 quote)) ((_2 closure)) ((_2 quote))) 
   (sym _0 _1 _2)
   (absento (closure _3)))))

   (define-relation
     (plop acc whole res data)
     (conde
       ((fresh
            (s)
          (== data `(,s))
      (== acc `(,s))
      (conde
       ((fresh
         (a d)
         (== s `(,a unquote d))
         (=/= d '())
         (fresh (m) (== res `(,m)) (plop s whole m s))))
       ((== 'q s) (== res `(q ',whole))))))
    ((fresh
      (a d)
      (=/= d '())
      (== `(,a unquote d) data)
      (fresh
       (p)
       (== res `(,a unquote p))
       (fresh (q) (== acc `(,a unquote q)) (plop q whole p d)))))))
   (run*
    (q)
    (fresh
     (x)
     (plop
      x
      x
      q
      '((define-relation
         (plop acc whole res data)
         (conde
          ((fresh
            (s)
            (== data `(,s))
            (== acc `(,s))
            (conde
             ((fresh
               (a d)
               (== s `(,a unquote d))
               (=/= d '())
               (fresh (m) (== res `(,m)) (plop s whole m s))))
             ((== 'q s) (== res `(q ',whole))))))
          ((fresh
            (a d)
            (=/= d '())
            (== `(,a unquote d) data)
            (fresh
             (p)
             (== res `(,a unquote p))
             (fresh (q) (== acc `(,a unquote q)) (plop q whole p d)))))))
        (run* (q) (fresh (x) (plop x x q)))))))

