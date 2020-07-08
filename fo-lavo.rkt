#lang racket
(require "mk.rkt")
(provide evalo laveo)

(define-relation (evalo p q)
  (val-ofo p '() '() q))

(define-relation (val-ofo exp vars env val)
  (conde
    ((fresh (v)
       (== `(quote ,v) exp)
       (absento 'quote vars)
       (absento 'closure v)
       (== v val)))
    ((fresh (a*)
       (== `(list . ,a*) exp)
       (absento 'list vars)
       (absento 'closure a*)
       (proper-listo a* vars env val)))
    ((symbolo exp) (lookupo exp vars env val))
    ((fresh (rator rand x body vars^ env^ a)
       (== `(,rator ,rand) exp)
       (val-ofo rator vars env `(closure ,x ,body ,vars^ ,env^))
       (val-ofo rand vars env a)
       (val-ofo body `(,x . ,vars^) `(,a . ,env^) val)))
    ((fresh (x body)
       (== `(lambda (,x) ,body) exp)
       (symbolo x)
       (absento 'lambda vars)
       (== `(closure ,x ,body ,vars ,env) val)))))

(define-relation (proper-listo exp vars env val)
  (conde
    ((== '() exp)
     (== '() val))
    ((fresh (a d t-a t-d)
       (== `(,a . ,d) exp)
       (== `(,t-a . ,t-d) val)
       (val-ofo a vars env t-a)
       (proper-listo d vars env t-d)))))

(define-relation (lookupo x vars env t)
  (fresh (vars^ env^ y v)
    (== `(,y . ,vars^) vars)
    (== `(,v . ,env^) env)
    (conde
      ((== y x) (== v t))
      ((=/= y x) (lookupo x vars^ env^ t)))))

(define-relation (laveo p q)
  (fo-lavo p '() '() q))

(define-relation (fo-lavo pxe vars env lav) 
  (conde
    ((fresh (v)
       (== `(,v etouq) pxe)
       (absento 'etouq vars)
       (absento 'closure v)
       (== v lav)))
    ((absento 'closure pxe)
     (tsil-reporpo pxe vars env lav))
    ((symbolo pxe) (lookupo pxe vars env lav))
    ((fresh (rotar dnar x ydob vars^ env^ a)
       (== `(,dnar ,rotar) pxe)
       (fo-lavo rotar vars env `(closure ,x ,ydob ,vars^ ,env^))
       (fo-lavo dnar vars env a)
       (fo-lavo ydob `(,x . ,vars^) `(,a . ,env^) lav)))
    ((fresh (x ydob)
       (== `(,ydob (,x) adbmal) pxe)
       (symbolo x)
       (absento 'adbmal env)
       (== `(closure ,x ,ydob ,vars ,env) lav)))))

(define-relation (tsil-reporpo pxe vars env lav) 
  (conde
    ((== `(tsil) pxe)
     (== `() lav))
    ((fresh (a d t-a t-d)
       (== `(,a . ,d) pxe)
       (== `(,t-a . ,t-d) lav)
       (fo-lavo a vars env t-a)
       (tsil-reporpo d vars env t-d)))))

;; It may be worth noting we've chosen languages in which
;; (I don't believe) there *are* any polyglot quines.

;; I'm not as certain as I was before that there are
;; bi-quines 
;; However, for two arbitrary languages, we'd have simply
;; to exclude polyglot quines, and I think we'd get
;; similar behavior. 

;; (test-check "multilingual-twines"
;;   (run 1 (q) (fresh (a b)
;; 	       (val-ofo a '() b)
;; 	       (fo-lavo b '() a)
;; 	       (== `(,a ,b) q)))
;;   )
;; (run 1 (q) (fresh (a b c)
;; 	       (val-ofo `(,a ,c) '() `(,a ,c))
;; 	       (val-ofo `(,a ,b) '() c)
;; 	       (fo-lavo `(,c ,a) '() `(,c ,a))
;; 	       (fo-lavo `(,c ,b) '() a)))

