;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: naive-bayes.rkt                                           ;;
;;                                                                        ;;
;;  Purpose: Naive Bayes classifier.                                      ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(require "parse-args.rkt")


; abbreviation
(define num->str number->string)


; Read in training data
(define naive-bayes-classifier
  (class object%
    
    (super-new)
    
    (field (freqs (make-hash)))
    (field (num-labels 0))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public  Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    ; train
    ; ex. (send this-model train 'file "data/greet.txt")
    (define/public (train . args)

      ; Create list of list of tokens
      (let
          ((toks (parse-train-toks args)))
        
        ;(display "tokens!!")
        ;(newline)
        ;(define (display-tokens sent)
        ;  (map (lambda (t) (display t) (display " " )) sent)
        ;  (newline))
        ;(map display-tokens toks)
        ;(newline)
        
        ; Get full list EXCEPT for final element
        (define (but-last lst)
          (reverse (cdr (reverse lst))))
        
        (define (get-label lst)
          (string->number (last lst)))
        
        ; vectorize features
        (let 
            ((features (map but-last  toks))
             (labels   (map get-label toks)))
        
          ; build model using vectorized features
          (count-feat-freqs features labels))
        
        ))
          
    
    ; predict
    ; ex. (send this-model predict 'file "data/greet.txt")
    (define/public (predict . args)

      ; Create list of feature lists
      (let
          ((X (parse-train-toks args)))
        
        ;(display "tokens!!")
        ;(newline)
        ;(define (display-tokens sent)
        ;  (map (lambda (t) (display t) (display " " )) sent)
        ;  (newline))
        ;(map display-tokens X)
        ;(newline)
        
        ; predict label for each instance
        (map (lambda (feats) (predict-label feats)) 
             X)
        
        ))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define/public (count-feat-freqs features labels)
      
      (define (update-freqs feature label)

        ; tail-recursive. updates one features and then recursively updates the rest
        ; (all for one given data point)
        ; NOTE: features look like: single-i-value-label 
        ;      (meanining a datapoint has a given label and the i-th feature has a given value
        (define (update-feature-i feats i)
          (if (null? feats)
              void
              (begin
                ;(displayln (list "i: " i "feat: " (car feats) "label: " label))
                (let 
                    ((single-feat (string-append "single-" (num->str i)
                                                       "-" (car feats)))
                     (  cond-feat (string-append   "cond-" (num->str i)
                                                       "-" (car feats)
                                                       "-" (num->str label)))
                     (label-name  (string-append  "label-" (num->str label))))
                  ;(displayln (hash-ref freqs single-feat 0))
                  (let
                      ((feat-i-single-count (hash-ref freqs single-feat 0))
                       (feat-i-cond-count   (hash-ref freqs   cond-feat 0)))
                    ;(displayln (list (list "i" i) (list "feat" (car feats)) (list "label" label) (list "single-count" feat-i-single-count)))
                    ;(displayln (list (list "i" i) (list "feat" (car feats)) (list "label" label) (list "  cond-count" feat-i-cond-count  )))
                    ;(newline)
                    (hash-set! freqs single-feat (add1 feat-i-single-count))
                    (hash-set! freqs   cond-feat (add1   feat-i-cond-count)))

                  )
                (update-feature-i (cdr feats) (+ i 1)))))
        
        ;(displayln (list "feature/label: " feature label))
        (update-feature-i feature 1))

      
      ; Update counts of labels
      (define (update-label-count label)
        (let
            ((label-name (string-append "label-" (num->str label))))
          (hash-set! freqs 
                     label-name
                     (add1 (hash-ref freqs label-name 0)))))

      (map update-label-count labels)
      (displayln freqs)
      (set! num-labels (length (hash->list freqs)))

      
      ; Visit each data point and their joint probabilities with labels
      (map update-freqs features labels)
      (displayln freqs)
      void)
    
    
    ; predict-label
    ; input:  an ordered list of features
    ; output: one of the legal labels (0 to k-1)
    (define/private (predict-label features)
      
      ;(displayln features)
      ;(displayln freqs)
      
      ; compute likelihood for given label
      (define (prob-of-label-i i)
        
        ; compute the conditional probability of the j-th feature (given a label)
        (define (conditional-log-prob j)
          ;(displayln (list "label" i "feature" j))
          
          (let
              ((single (hash-ref freqs (string-append "single-" (num->str (add1 j))
                                                            "-" (num->str (list-ref features j)))))
               (joint (hash-ref freqs (string-append    "cond-" (num->str (add1 j))
                                                            "-" (num->str (list-ref features j))
                                                            "-" (num->str i)))))
            (log (/ single joint))))
        
        ;(newline)
        ;(displayln i)
        (let
            ; should technically divide by total num of labels, but that never changes
            ; TODO - switch to log space (will be easy to do, but debugging is easier with probs
            ((log-prior  (log (hash-ref freqs (string-append "label-" (num->str i)))))
             (posteriors (map conditional-log-prob (range (length features)))))
          ;(displayln prior)
          ;(displayln posts)
          (+ log-prior (foldl + 0 posteriors))))
      
      (argmax prob-of-label-i (range num-labels)))
    
))



; Instantiate classifier
(define nb-classifier (new naive-bayes-classifier))

; Train model
(send nb-classifier train 'file "data/annotated.txt")

; TODO - make feature vectorizer

; Predict classification
(send nb-classifier predict 'list '((0 0) (0 1) (1 0) (1 1)))
