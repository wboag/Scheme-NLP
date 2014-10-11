;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: evaluate-stem.rkt                                         ;;
;;                                                                        ;;
;;  Purpose: Evaluate predictions from stemmer.                           ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#lang racket


; Evaluation
(define (evaluate-porter text-file gold-file pred-file)

  (define (zip text gold pred)
    (list text gold pred))
  
  (define (correct-stem? text-gold-pred)
    (equal? (second text-gold-pred) (third text-gold-pred)))
  
  (define (incorrect-stem? text-gold-pred)
    (not (correct-stem? text-gold-pred)))
  
  (let 
      ; Zip all three lists of data together
      ((total  (map zip (file->list text-file)
                        (file->list gold-file)
                        (file->list pred-file))))

    ; Display incorrect stems
    (display (filter incorrect-stem? total))
    
    ; Statistics of performance
    (newline)
    (display "correct: ")
    (display (length (filter correct-stem? total)))
    (newline)
    (display "total:   ")
    (display (length                         total ))
  
  ))


; Run evaluation
(evaluate-porter "data/vocab.txt" 
                 "data/output.txt" 
                 "data/predictions.txt")
