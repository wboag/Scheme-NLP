;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: tokenizer.rkt                                             ;;
;;                                                                        ;;
;;  Purpose: Break sentences into word tokens.                            ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(provide file->tokens)
(provide file->sentence-tagged-tokens)

(provide sentence->tokens)
(provide sentence->sentence-tagged-tokens)

   
; Read file and parse into sentences
(define (file->tokens file-name)  
  (read-file (open-input-file (string->path file-name)) #f))
    

; Read file and parse into sentences
(define (file->sentence-tagged-tokens file-name)  
  (read-file (open-input-file (string->path file-name)) #t))


; read each line from the file
(define (read-file file-id tags?)
  (let
      ((line (read-line file-id)))
    (if (eq? line eof)
        '()
        (let*
            ((toks (sentence->tokens line))
             (result (if tags?
                         (append '("<s>") toks '("</s>"))
                         toks)))
          (cons result (read-file file-id tags?))))))


(define (sentence->tokens sent)
  (string-split sent " "))


(define (sentence->sentence-tagged-tokens sent)
  (append '("<s>") (string-split sent " ") '("</s>")))