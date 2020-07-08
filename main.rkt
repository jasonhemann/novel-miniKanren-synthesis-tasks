#lang racket
(require (except-in rackunit fail) "mk.rkt" "fo-lavo.rkt")

(test-equal? "Query for a Quine"
 (run 1 (q) (evalo q q))
'((((lambda (_0) (list _0 (list 'quote _0)))
   '(lambda (_0) (list _0 (list 'quote _0))))
  (=/= ((_0 clos)) ((_0 list)) ((_0 quote)))
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
'(((''''((lambda (_0) (list 'quote (list _0 (list 'quote _0))))
        '(lambda (_0) (list 'quote (list _0 (list 'quote _0)))))
   '''((lambda (_0) (list 'quote (list _0 (list 'quote _0))))
       '(lambda (_0) (list 'quote (list _0 (list 'quote _0)))))
   ''((lambda (_0) (list 'quote (list _0 (list 'quote _0))))
      '(lambda (_0) (list 'quote (list _0 (list 'quote _0)))))
   '((lambda (_0) (list 'quote (list _0 (list 'quote _0))))
     '(lambda (_0) (list 'quote (list _0 (list 'quote _0)))))
   ((lambda (_0) (list 'quote (list _0 (list 'quote _0))))
    '(lambda (_0) (list 'quote (list _0 (list 'quote _0))))))
  (=/= ((_0 clos)) ((_0 list)) ((_0 quote)))
  (sym _0))))

(test-equal? "2-cycle quine relay"
 (run 1 (q) (fresh (p) (laveo p q) (evalo q p)))
'((((lambda (_0) (list (list _0 (list 'quote _0)) 'etouq))
    '(lambda (_0) (list (list _0 (list 'quote _0)) 'etouq)))
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
  (run 3 (q) 
    (fresh (p r s)
      (== q `(,s ,r))
      (absento p s)
      (evalo `(,p ,r) `(,p ,r))
      (evalo `(,s ,r) `(,p ,r))))
'((((lambda (_0) (list _0 (list 'quote _0)))
    '(lambda (_1) (list _1 (list 'quote _1))))
   (=/=
    ((_0 closure))
    ((_0 list))
    ((_0 quote))
    ((_1 closure))
    ((_1 list))
    ((_1 quote)))
   (sym _0 _1))
  ((((lambda (_0) (lambda (_1) (list _1 (list 'quote _1)))) '_2)
     '(lambda (_3) (list _3 (list 'quote _3))))
    (=/=
     ((_0 lambda))
     ((_0 list))
     ((_0 quote))
     ((_1 closure))
     ((_1 list))
     ((_1 quote))
     ((_2 (lambda (_3) (list _3 (list 'quote _3)))))
     ((_3 closure))
     ((_3 list))
     ((_3 quote)))
    (sym _0 _1 _3)
    (absento ('(lambda (_3) (list _3 (list 'quote _3))) _2) (closure _2)))
  (((lambda (_0)
      (list
       (list 'lambda '(_1) '(list _1 (list 'quote _1)))
       (list 'quote _0)))
    '(lambda (_1) (list _1 (list 'quote _1))))
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
  (((lambda (_0) (lambda (_1) (list _0 (list 'quote _0))))
    '(lambda (_0) (lambda (_1) (list _0 (list 'quote _0)))))
   (=/=
    ((_0 _1))
    ((_0 closure))
    ((_0 lambda))
    ((_0 list))
    ((_0 quote))
    ((_1 closure))
    ((_1 list))
    ((_1 quote)))
   (sym _0 _1))))

(test-equal? "3-cycle of distinct ``stuttered'' quasi-quines"
  (run 1 (t)
    (fresh (a b c) 
      (== t `((,a ,b) (,b ,c) (,c ,a)))
      (=/= a b) (=/= b c) (=/= c a)
      (evalo `(,a ,b) c)
      (evalo `(,b ,c) a)
      (evalo `(,c ,a) b)))
'((((lambda (_0)
      '(lambda (_1)
         (list
          'lambda
          '(_2)
          (list
           'quote
           (list 'lambda '(_0) (list 'quote (_1 '_3)))))))
    (lambda (_2)
      '(lambda (_0)
         '(lambda (_1)
            (list
             'lambda
             '(_2)
             (list
              'quote
              (list 'lambda '(_0) (list 'quote (_1 '_3))))))))
    (lambda (_1)
      (list
       'lambda
       '(_2)
       (list 'quote (list 'lambda '(_0) (list 'quote (_1 '_3)))))))
   (=/= ((_0 closure)) ((_0 quote)) ((_1 closure)) ((_1 list)) 
        ((_1 quote)) ((_2 closure)) ((_2 quote))) 
   (sym _0 _1 _2)
   (absento (closure _3)))))

(test-case "Irresistable"
  (define relation
    '(define-relation
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
         (fresh (q) (== acc `(,a unquote q)) (plop q whole p d))))))))
  (define query
    '(run*
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
          (run* (q) (fresh (x) (plop x x q))))))))
  (check-equal?
   (equal? (map eval (list relation query))
           (map eval (car (map eval (list relation query)))))))

