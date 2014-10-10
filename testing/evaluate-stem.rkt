#lang racket


; Evaluation
(define (evaluate-porter unstemmed-file stemmed-file)
  (let
      ((unstemmed-port (open-input-file (string->path unstemmed-file)))
       (  stemmed-port (open-input-file (string->path   stemmed-file)))
       (recall 0)
       (precision 0))

    ; Run through file line-by-line and count correct/incorrect counts
    (define (evaluate-helper)
      (let
          ((unstemmed-w (read-line unstemmed-port))
           (  stemmed-w (read-line   stemmed-port)))
        (if (or (equal? unstemmed-w eof)
                (equal?   stemmed-w eof))
            (list 0 0)  ; correct, incorrect
            (let*
                ((eval-rest (evaluate-helper))
                 (  correct  (car  eval-rest))
                 (incorrect  (cadr eval-rest)))
              (if (equal? stemmed-w unstemmed-w)
                  (list (+ 1 correct)      incorrect )
                  (list      correct  (+ 1 incorrect)))))))

    ; Get counts of correct,incorrect
    (evaluate-helper)))


; Run evaluation
(evaluate-porter "data/vsample.txt" "data/osample.txt")
