#!/usr/bin/env racket

#lang racket

(require racket/control)

(define-syntax-rule (eta f)
  (lambda (x) (f x)))

;; primitives

(define (run f inp)
  (reset (println (f inp))))

(define ((seq/2 a b) inp)
  (shift c
         (reset (c (a inp)))
         (reset (c (b inp)))))

(define (empty/0 inp)
  (shift c (void)))

(define/match (iter/1 inp)
  [((? list?))
   (shift c (for ([v inp])
              (c v)))]
  [((? hash?))
   (shift c (for ([(k v) inp])
              (c v)))])

(define ((array/1 f) inp)
  (let ([lst '()])
    (reset (set! lst (cons (f inp) lst)))
    (reverse lst)))

(define ((reduce gen init proc) inp)
  (define acc (init inp))
  (reset (let ([x (gen inp)])
           (set! acc ((proc acc) x))))
  acc)

;; regular functions

(define (((-op op) . args) inp)
  (apply op (map (lambda (a) (a inp)) args)))

(define/match (-plus x y)
  [('null _) y]
  [(_ 'null) x]
  [((? number?) (? number?)) (+ x y)])

(define +/2 (-op -plus))

(define ((if/3 c t f) inp)
  (if (c inp)
      (t inp)
      (f inp)))

(define (iter?/1 inp)
  (if (or (list? inp) (hash? inp))
      (iter/1 inp)
      (empty/0 inp)))

;; stdlib

(define (map/1 f)
  (array/1 (compose f iter/1)))

(define (select/1 f)
  (if/3 f identity empty/0))

(define add/0
  (reduce iter/1 (const 'null)
          (lambda (x)
            (+/2 identity (const x)))))

(define (recurse/1 f)
  (seq/2 identity (compose (eta (recurse/1 f)) f)))

(define recurse/0 (recurse/1 iter?/1))

;; examples

(run (map/1 add1) '(1 2 3))
(run add/0 '(1 2 3))
(run recurse/0 '((1 2) (3 4)))
