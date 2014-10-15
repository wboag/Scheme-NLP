;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: tokenizer.rkt                                             ;;
;;                                                                        ;;
;;  Purpose: Break sentences into word tokens.                            ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(provide tokenize)

    
; Dummy tokenizer for now.
(define (tokenize sent)
  (append '("<s>")
          (string-split sent " ")
          '("</s>")))

