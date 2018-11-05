; Kevin Gray
; CSC 173
; Project 3
#lang racket

(define (my_append x y)
  (if (null? x) y
      (cons (car x) (my_append (cdr x) y))))

(define (my_reverse lst)
  (my_reverse-h lst '()))
(define (my_reverse-h lst acc)
  (if (null? lst) acc
      (my_reverse-h (cdr lst) (cons (car lst) acc))))

(define (my_member c set)
  (if (null? set) #f
      (if (equal? c (car set)) #t
          (my_member c (cdr set)))))

(define (cardinality set)
  (cardinality-h set 0))
(define (cardinality-h set i)
  (if (null? set) (+ 0 i)
      (cardinality-h (cdr set) (+ 1 i))))

(define (my_insert c set)
  (if (my_member c set) set
      (cons c set)))

(define (factorial x)
  (factorial-h x 1))
(define (factorial-h x i)
  (if (< x 1) '()
      (if (equal? x 1) i
          (factorial-h (- x 1) (* i x)))))

(define (right-tri a b c)
  (if (equal? (+ (* a a) (* b b)) (* c c)) #t
      (if (equal? (+ (* a a) (* c c)) (* b b)) #t
          (if (equal? (+ (* c c) (* b b)) (* a a)) #t #f))))

(define (my_abs x)
  (if (< x 0) (+ (* -2 x) x) x))

(define (find_factors x)
  (find_factors-h x 1 '()))
(define (find_factors-h x i acc)
  (if (< i x)
      (if (zero? (remainder x i)) (find_factors-h x (+ 1 i) (cons i acc))
          (find_factors-h x (+ 1 i) acc))
      acc))

(define (perfect? x)
  (perfect-h x (find_factors x) 0))
(define (perfect-h x lst acc)
  (if (null? lst)
      (if (equal? x acc) #t #f)
      (perfect-h x (cdr lst) (+ (car lst) acc))))

(define (abundant? x)
  (abundant-h x (find_factors x) 0))
(define (abundant-h x lst acc)
  (if (null? lst)
      (if (> acc x) #t #f)
      (abundant-h x (cdr lst) (+ (car lst) acc))))

(define (deficient? x)
  (deficient-h x (find_factors x) 0))
(define (deficient-h x lst acc)
  (if (null? lst)
      (if (< acc x) #t #f)
      (deficient-h x (cdr lst) (+ (car lst) acc))))
