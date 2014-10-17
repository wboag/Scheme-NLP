;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: parse.rkt                                                 ;;
;;                                                                        ;;
;;  Purpose: Parse a list of arguments.                                   ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket


(require "tokenizer.rkt")


(provide parse-train-toks)


(define (parse-train-toks args)

  ; parse arguments & build list of list of tokens
  (let*
      ((training-file (memq 'file args))
       (training-list (memq 'list args))
       (tags?         (memq 'tags args)))
    
    (cond
      
      ((and (not training-file) (not training-list))
       (error "Must provide either training file or training list"))
      
      ((and training-file training-list)
       (error "Cannot provide both training file and training list"))
      
      (training-file
       (if (= (length training-file) 1)
           (error "Must specify training file")
           (if tags?
               (file->sentence-tagged-tokens (cadr training-file))
               (file->tokens                 (cadr training-file)))))
      
      (training-list
       (if (= (length training-list) 1)
           (error "Must specify training list")
           (cadr training-list))))))