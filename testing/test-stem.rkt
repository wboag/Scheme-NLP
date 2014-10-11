;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: test-stem.rkt                                             ;;
;;                                                                        ;;
;;  Purpose: Run stemmer on large vocabulary                              ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(require "../stem.rkt")


; Evaluation
(define (read-data vocab-file)
  (let
      ((data-port (open-input-file (string->path vocab-file))))

    ; Run through file line-by-line and count accumulate data
    (define (read-data-helper result)
      (let
          ((w (read-line data-port)))
        (if (equal? w eof)
            (reverse result)
            (read-data-helper (cons w result)))))

    ; Get counts of correct,incorrect
    (read-data-helper '())))


; Read data
(define data (read-data "data/vocab.txt"))


; Stem words
(define stems (map porter-stem data))


; Output
(define out (string-join stems "\n"))
(display out)
(newline)




; Demo
;(define words '("CARESSES"
;                "PONIES"
;                "TIES"
;                "CARESS"
;                "CATS"
;                "FEED"
;                "AGREED"
;                "PLASTERED"
;                "BLED"
;                "MOTORING"
;                "SING"
;                "CONFLATED"
;                "TROUBLED"
;                "SIZED"
;                "HOPPING"
;                "TANNED"
;                "FALLING"
;                "HISSING"
;                "FIZZED"
;                "FAILING"
;                "FILING"
;                "HAPPY"
;                "SKY"))

;(define words '("RELATIONAL"
;                "CONDITIONAL"
;                "RATIONAL"
;                "VALENCI"
;                "HESITANCI"
;                "DIGITIZER"
;                "CONFORMABLI"
;                "RADICALLI"
;                "DIFFERENTLI"
;                "VILELI"
;                "ANALOGOUSLI"
;                "VIETNAMIZATION"
;                "PREDICATION"
;                "OPERATOR"
;                "FEUDALISM"
;                "DECISIVENESS"
;                "HOPEFULNESS"
;                "CALLOUSNESS"
;                "FORMALITI"
;                "SENSITIVITI"
;                "SENSIBILITI"))

;(define words '("TRIPLIC"
;                "FORMATIVE"
;                "FORMALIZE"
;                "ELECTRICITI"
;                "ELECTRICAL"
;                "HOPEFUL"
;                "GOODNESS"))

;(define words '("REVIVAL"
;                "ALLOWANCE"
;                "INFERENCE"
;                "AIRLINER"
;                "GYROSCOPIC"
;                "ADJUSTABLE"
;                "DEFENSIBLE"
;                "IRRITANT"
;                "REPLACEMENT"
;                "ADJUSTMENT"
;                "DEPENDENT"
;                "ADOPTION"
;                "HOMOLOGOU"
;                "COMMUNISM"
;                "ACTIVATE"
;                "ANGULARITI"
;                "HOMOLOGOUS"
;                "EFFECTIVE"
;                "BOWDLERIZE"))


;(define words '("PROBATE"
;                "RATE"
;                "CEASE"
;                "CONTROLL"
;                "ROLL"))

;(define words (map (lambda (s) (symbol->string s))
;                   (file->list "data/vocab-sample.txt")))
