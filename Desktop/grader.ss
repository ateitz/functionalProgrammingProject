;;#lang racket/load
(load "include.ss")
(load "document.ss")
(load "dictionary.ss")
(load "decode.ss")

;;helper function to check equal
(define checkEqual
  (lambda (sol student pt)
    (if (equal? sol student) pt 0)))

;;TEST CASES / encode-n / *** 5 points for each / total:15
(define add15 (encode-n 15))
(define add10 (encode-n 10))
(define add5 (encode-n 5))

(define (testAdd5) (checkEqual '(g j j) (add5 '(b e e)) 5))
(define (testAdd10) (checkEqual '(d o x) (add10 '(t e n)) 5))
(define (testAdd15) (checkEqual '(o d d) (add15 '(z o o)) 5))

;;TEST CASES / spell-checker / *** 5 points for each / total:15

(define (testSP1) (checkEqual #t (spell-checker '(b e e)) 5))
(define (testSP2) (checkEqual #f (spell-checker '(w o r l e)) 5))
(define (testSP3) (checkEqual #t (spell-checker '(t r e e)) 5))

;;TEST CASES / encode-d / *** 11 points for each / total: 22
(define test-d '(
                 ((h o w)(t h e)(w o r l d)(f e e l)(t h e)(h e a t))
                 ((e a r t h))
                 ))
(define test-d2 '(
                  ((a)(b e e)(i s)(s e e n)(o n)(a)(g r e e n)(t e a)(t r e e))
                  ((t e a)(t r e e))
                  ))
(define test-encoded-d '(
                         ((m t b) (y m j) (b t w q i) (k j j q) (y m j) (m j f y)) 
                         ((j f w y m))))
(define test-encoded-d2 '(((f) (g j j) (n x) (x j j s) (t s) (f) (l w j j s) (y j f) (y w j j))
			 ((y j f) (y w j j))))
;;;;;;;;;;;;;;;;
(define (testEncode1) (checkEqual test-encoded-d (encode-d test-d add5) 11))
(define (testEncode2) (checkEqual test-encoded-d2 (encode-d test-d2 add5) 11))

;;TEST CASES / decoder generator / *** 10 points for each / total:40
(define decoderSP1 (Gen-Decoder-A (car test-encoded-d)))
(define decoderFA1 (Gen-Decoder-B (car test-encoded-d)))
(define decoderSP2 (Gen-Decoder-A (car test-encoded-d2)))
(define decoderFA2 (Gen-Decoder-B (car test-encoded-d2)))

(define (decoderGen1) (checkEqual '(h o w) (decoderSP1 '(m t b)) 10))
(define (decoderGen2) (checkEqual '(t h e) (decoderFA1 '(y m j)) 10))
(define (decoderGen3) (checkEqual '(t h i s) (decoderSP2 '(y m n x)) 10))
(define (decoderGen4) (checkEqual '(b e e) (decoderFA2 '(g j j)) 10))


;;TEST CASES / code breaker / *** 2 points for each / total: 8
(define (testBker1) (checkEqual test-d (Code-Breaker test-encoded-d decoderSP1) 2))
(define (testBker2) (checkEqual test-d (Code-Breaker test-encoded-d decoderFA1) 2))
(define (testBker3) (checkEqual test-d2 (Code-Breaker test-encoded-d2 decoderSP2) 2))
(define (testBker4) (checkEqual test-d2 (Code-Breaker test-encoded-d2 decoderFA2) 2))

;;main run
(define tests
  (list testAdd5
	testAdd10
        testAdd15
        testSP1
        testSP2
	testSP3
        testEncode1
        testEncode2
        decoderGen1
        decoderGen2
        decoderGen3
        decoderGen4
        testBker1
        testBker2
        testBker3
        testBker4
        ))

(define (run-test test)
  (with-handlers
   ((exn:fail?
     (lambda (ex)
       "err")))
   (test)))

(define (run-tests)
  (map run-test tests))

;;display the results
(let* ((results (run-tests))
       (res-string (string-join (map (lambda (x) (if (equal? "err" x) "0" (format "~a" x))) results) "\t\n"))
       (finalScore (map (lambda (x) (if (equal? "err" x) 0 x)) results))
       (total (reduce + finalScore 0)))
  (display res-string)
  (display "\t\nTotal Points: ")
  (display total)
  (display "/100")
  (newline))
