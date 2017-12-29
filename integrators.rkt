#lang racket

(require racket/future)

(provide riemann-bigint riemann simpson-bigint simpson trapazoid-bigint trapazoid)
(provide err cm pexec timeexec)

(define (riemann-bigint func low up n)
  (let ([x (/ (- up low) n)] [sum 0])
    (for ([i (in-range 0 n)])
      (set! sum (+ sum (* (func (+ low (/ x 2) (* x i))) x))))
    sum))

(define (riemann func low up n)
  (let ([x (/ (- up low) n)] [sum 0.0])
    (for ([i (in-range 0.0 n)])
      (set! sum (+ sum (* (func (+ low (/ x 2.0) (* x i))) x))))
    sum))

(define (trapazoid-bigint func low up n)
  (let ([x (/ (- up low) n)] [sum 0])
    (for ([i (in-range 0 n)])
      (set! sum (+ sum (* (/ (+ (func (+ low (* x i))) (func (+ low (* x (+ i 1))))) 2) x))))
    sum))

(define (trapazoid func low up n)
  (let ([x (/ (- up low) n)] [sum 0.0])
    (for ([i (in-range 0.0 n)])
      (set! sum (+ sum (* (/ (+ (func (+ low (* x i))) (func (+ low (* x (+ i 1.0))))) 2.0) x))))
    sum))

(define (simpson-bigint func low up n)
  (let ([x (/ (- up low) n)] [sum (+ (func low) (func up))])
    (for ([i (in-range 1 n 2)])
      (set! sum (+ sum (* 4 (func (+ low (* i x)))))))
    (for ([i (in-range 2 (- n 1) 2)])
      (set! sum (+ sum (* 2 (func (+ low (* i x)))))))
    (* sum (/ x 3))))

(define (simpson func low up n)
  (let ([x (/ (- up low) n)] [sum (+ (func low) (func up))])
    (for ([i (in-range 1.0 n 2.0)])
      (set! sum (+ sum (* 4.0 (func (+ low (* i x)))))))
    (for ([i (in-range 2.0 (- n 1.0) 2.0)])
      (set! sum (+ sum (* 2.0 (func (+ low (* i x)))))))
    (* sum (/ x 3.0))))

(define (err actual value)
  (abs (- actual value)))

(define (cm num mult)
  (if (< (/ mult 2) (modulo num mult)) (+ num (- mult (modulo num mult))) (- num (modulo num mult))))

(define (pexec int threads func low up n)
  (apply + (map touch (build-list threads (λ (x) (future (λ () (int func (* x (/ (- up low) threads)) (* (+ x 1) (/ (- up low) threads)) (/ n threads)))))))))

(define (timeexec func)
  (let ([stime (current-inexact-milliseconds)])
    (func)
    (- (current-inexact-milliseconds) stime)))
