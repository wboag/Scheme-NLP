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
(define (porter-stem w)
  w)



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