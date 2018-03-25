; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2016                              *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ctv", "vtc",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "dictionary.ss")

;; (load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
    
    (if (member w dictionary)
        #t #f) ;; *** FUNCTION BODY IS MISSING *** 
    ))
(define encode-p
  (lambda (o n)
    (map spell-checker (map (encode-n n) o ))
    )
  )
(define tally
  (lambda (n)
    
    (reduce + (map (lambda (w)(if (eqv? #t w)
                                  1
                                  0
                                  )) n) 0)     
    )
  )

(define convert
  (lambda (p n)
    (tally(map (lambda (x)  (spell-checker x)) (map(encode-n n) p))))
  )

(define listMaker
  (lambda (l o)
    (tally (encode-p l o))
    )
  )
(define findIndexNum 
  (lambda (l n)
    (if (null? l) 0
        (if (equal? n (car l))
            0
            (+ 1 (findIndexNum(cdr l) n)) ))
    ))
(define CountOccurances
  (lambda (p l)
    (reduce + (map (lambda (y)(reduce + (map (lambda (x) (if (= (ctv x) l)
                                                             1
                                                             0)) y)0)) p)0)
        
    )
  )
(define shiftToA
  (lambda (y)
    (+ (- 26 y) 0)
    ))
(define shiftToE
  (lambda (y)
    (+ (- 26 y) 4)
    ))
(define shiftToT
  (lambda (y)
    (+ (- 26 y) 19)
    ))


(define TryTwentySix
  (lambda (p)
    (apply max
           (list
            (CountOccurances p 0)
            (CountOccurances p 1)
            (CountOccurances p 2)
            (CountOccurances p 3)
            (CountOccurances p 4)
            (CountOccurances p 5)
            (CountOccurances p 6)
            (CountOccurances p 7)
            (CountOccurances p 8)
            (CountOccurances p 9)
            (CountOccurances p 10)
            (CountOccurances p 11)
            (CountOccurances p 12)
            (CountOccurances p 13)
            (CountOccurances p 14)
            (CountOccurances p 15)
            (CountOccurances p 16)
            (CountOccurances p 17)
            (CountOccurances p 18)
            (CountOccurances p 19)
            (CountOccurances p 20)
            (CountOccurances p 21)
            (CountOccurances p 22)
            (CountOccurances p 23)
            (CountOccurances p 24)
            (CountOccurances p 25)
            )
           )
    )
  )
(define TryTwentySixTwo
  (lambda (p)
    
    (list
     (CountOccurances p 0)
     (CountOccurances p 1)
     (CountOccurances p 2)
     (CountOccurances p 3)
     (CountOccurances p 4)
     (CountOccurances p 5)
     (CountOccurances p 6)
     (CountOccurances p 7)
     (CountOccurances p 8)
     (CountOccurances p 9)
     (CountOccurances p 10)
     (CountOccurances p 11)
     (CountOccurances p 12)
     (CountOccurances p 13)
     (CountOccurances p 14)
     (CountOccurances p 15)
     (CountOccurances p 16)
     (CountOccurances p 17)
     (CountOccurances p 18)
     (CountOccurances p 19)
     (CountOccurances p 20)
     (CountOccurances p 21)
     (CountOccurances p 22)
     (CountOccurances p 23)
     (CountOccurances p 24)
     (CountOccurances p 25)
     )
     
    )
  )
(define findMaxOccr 
  (lambda (p)
    (if (null? p) 0
        (findIndexNum
         (TryTwentySixTwo p) (TryTwentySix p)))
    )
  )

(define findBestOffset
  (lambda (p)
    (findIndexNum
     (list 
      (convert p (shiftToA(findMaxOccr p)))
      (convert p (shiftToE(findMaxOccr p)))
      (convert p (shiftToT(findMaxOccr p)))
      )
     (apply max
            (list 
             (convert p (shiftToA(findMaxOccr p)))
             (convert p (shiftToE(findMaxOccr p)))
             (convert p (shiftToT(findMaxOccr p)))
             )
            )
     )
    )
  )
(define findBestOffsetTwo
  (lambda (p)
    (if (eqv? 0 (findBestOffset p))
        (shiftToA(findMaxOccr p))
          (if (eqv? 1 (findBestOffset p))
           (shiftToE(findMaxOccr p))
            (if (eqv? 2 (findBestOffset p))
                 (shiftToT(findMaxOccr p))('())
    )   
    )
    )
   )
  )
(define numList
  (lambda (p)
    (list
     (listMaker p 0)
     (listMaker p 1)
     (listMaker p 2)
     (listMaker p 3)
     (listMaker p 4)
     (listMaker p 5)
     (listMaker p 6)
     (listMaker p 7)
     (listMaker p 8)
     (listMaker p 9)
     (listMaker p 10)
     (listMaker p 11)
     (listMaker p 12)
     (listMaker p 13)
     (listMaker p 14)
     (listMaker p 15)
     (listMaker p 16)
     (listMaker p 17)
     (listMaker p 18)
     (listMaker p 19)
     (listMaker p 20)
     (listMaker p 21)
     (listMaker p 22)
     (listMaker p 23)
     (listMaker p 24)
     (listMaker p 25)                  
     )
    )
  )

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input=a word, output=encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
      (map vtc (map (lambda (y) (modulo y 26)) (map (lambda(x) (+ n x))(map ctv w))))
      ; (modulo(vtc (+ ctv(w) n)) 26);; *** FUNCTION BODY IS MISSING ***
      )))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    (map (lambda(x) (map encoder x)) d);; *** FUNCTION BODY IS MISSING ***
    )
  )
 
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
    (define decoder
      (lambda (n)
        (encode-n n)))

    (decoder (findIndexNum (numList p) (apply max (numList p))))))
    

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
    (define TIMETODECODE
      (lambda (n)
        (encode-n n)));; *** FUNCTION BODY IS MISSING ***
    (TIMETODECODE (findBestOffsetTwo p))
    ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d encoder)
    (map (lambda(x) (map encoder x)) d);; *** FUNCTION BODY IS MISSING ***
    ))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)
