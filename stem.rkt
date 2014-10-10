;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: stem.rkt                                                  ;;
;;                                                                        ;;
;;  Purpose: Implementation of Porter stemming.                           ;;
;;                                                                        ;;
;;            http://tartarus.org/~martin/PorterStemmer/                  ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#lang racket


 ; Porter Stemmer
(define porter-object
  (class object%
    
    (super-new)

    ; store internal data to avoid repeated computations
    (field (buffer-key    ""))
    (field (global-buffer ""))

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public  Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    (define/public (stem w)
      w)

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    (define/public (consonant? str ind)
      ; Note: Assumes word cannot ever have "yy"
      (not
       (regexp-match? #rx"(a|e|i|o|u|(?<=[^aeiou])y)"
                      (string-downcase (substring str ind (+ 1 ind))))))


    (define/public (get-m s)
      ; Number of inner VC pairs => half of the length of the inner list
      (- (/ (length (CV-split s)) 2) 1))
    
    
    ; parition word into alternating list of consontant and vowel substrings
    ; Note: Always begins with consontant strng & ends with vowel string
    ; ex. "alternating" => '("" "a" "lt" "e" "rn" "a" "t" "i" "ng" "")
    (define/public (CV-split str)
      
      ; "alterating" => '("" "a" "lternating")
      (define (get-leading-CV s)
        (let
            ((   C-regex "^((?i:y?[^aeiouy]*))")
             (   V-regex "((?i:(?:y[aeiou]*|[aeiou]*)))" )
             (rest-regex "(.*)"        ))                
          (cdr (regexp-match (pregexp (string-append    C-regex
                                                        V-regex
                                                        rest-regex))
                             s))))
      
      ; iterative process to split string by repeatedly calling get-leading-CV
      (define (CV-split-helper s result)
        (if (= 0 (string-length s))
            (begin
              (set! buffer-key       str) ; memo-ize
              (set! global-buffer result) ; so you don't keep recomputing
              result)
            (let*
                ((C-V-rest (get-leading-CV s))
                 (C        (first  C-V-rest) )
                 (V        (second C-V-rest) )
                 (rest     (third  C-V-rest) ))
              (CV-split-helper rest (append result (list C V) )))))
      
      ; Break down str into C-Vs
      ; Check if memo-ized
      (if (equal? str buffer-key)
          global-buffer
          (CV-split-helper str '())))
          
))



; Instantiate object
(define porter (new porter-object))


; Mappable function
(define (porter-stem w)
  (send porter stem w))


; Demo
(define words '("CONNECT"
                "CONNECTED"
                "CONNECTING"
                "CONNECTION"
                "CONNECTIONS"))

;(display words)
;(newline)
;(display (map porter-stem words))
(display              "CONNECT" )
(newline)
(display (porter-stem "CONNECT"))
(newline)