#lang racket


(require "../ngrams.rkt")


; Instantiate model
(define model-a (new ngram-model (n 2)))
(define model-b (new ngram-model (n 1)))

; Train model on input data
(send model-a train 'file "../data/greet.txt")

; Predict probabiity of a sentence
(newline)
(display "<probability>")
(newline)
(display (send model-a probability "John read Moby Dick"))
(newline)
(display (send model-a probability "Cher read a book"))
(newline)
(display (send model-a probability "Cher read a book" 'smooth 'additive 1))
(newline)

; Generate a random sequence of text
(newline)
(display "<generate>")
(newline)
(display (send model-a generate 10 '("<s>")))
(newline)

; Get frequencies
(newline)
(display "<get-frequencies>")
(newline)
(display (send model-a get-frequency '("John" "read") 'smooth 'additive 1))
(newline)
(display (send model-a get-frequency '("Cher" "read") 'smooth 'additive .03))
(newline)
(display (send model-a get-frequency '("a" "book")))
(newline)
(display (send model-a get-frequency '("John") 'smooth 'additive 1))
(newline)