;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: naive-bayes.rkt                                           ;;
;;                                                                        ;;
;;  Purpose: Naive Bayes classifier.                                      ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(require "parse-args.rkt")
;(require "ngrams.rkt")



; Read in training data
(define naive-bayes-classifier
  (class object%
    
    (super-new)
    
        
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public  Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    ; train: [train-file]
    ; ex. (send this-model train "data/greet.txt")
    (define/public (train . args)

      ; Create list of list of tokens
      (let
          ((toks (parse-train-toks args)))
        
        (display "tokens!!")
        (newline)
        (define (display-tokens sent)
          (map (lambda (t) (display t) (display "-" )) sent)
          (newline))
        (map display-tokens toks)
        (newline)))
           
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
))



; Instantiate classifier
(define nb-classifier (new naive-bayes-classifier))

; Train model
;(send nb-classifier train 'file "data/greet.txt")

; Build ngram model for data
;(define ngrams-model (new ngram-model (n 2)))
;(send ngrams-model train)


; Get frequencies
;(newline)
;(display "<get-frequencies>")
;(newline)
;(display (send ngrams-model get-frequency '("John" "read") 'smooth 'additive 1))
;(newline)
;(display (send ngrams-model get-frequency '("Cher" "read") 'smooth 'additive .03))
;(newline)
;(display (send ngrams-model get-frequency '("a" "book")))
;(newline)
;(display (send ngrams-model get-frequency '("John") 'smooth 'additive 1))
;(newline)