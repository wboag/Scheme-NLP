;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Scheme-NLP: ngrams.rkt                                                ;;
;;                                                                        ;;
;;  Purpose: Simple n-gram model for natural language                     ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket

(require racket/file)
(require racket/string)
(require racket/set)
(require math/flonum)         ; for log space calculations
(require math/distributions)  ; for text generation

(define my-in-port   (open-input-file (string->path "data/greet.txt")))


(define (build-ngram-model n)
   
  ; data member variables
  (let 
      ((freqs       (make-hash))   ; Buzzword: Hash Table
       (vocab     (mutable-set))
       (epsilon1  (expt 10 -20))
       (epsilon2  (expt 10 -10)))
    
    
    ; Object interface
    ; Buzzword: Message Passing
    ; Buzzword: Variable-length argument
    (define (obj-interface msg . args)
      (cond 
        
        ((eq? msg 'hash ) freqs)
        
        ; prob: <sentence> [smoothing-delta]
        ((eq? msg 'prob )
         (if (= (length args) 1)
             (probability (string-split (first args) " ") 0)
             (probability (string-split (first args) " ") (second args))))
        
        ; generate: <k>
        ((eq? msg 'generate)
         (cond
           ((=  (length args) 0) (generate  3              '("<s>") ))
           ((=  (length args) 1) (generate (car args)      '("<s>") ))
           ((>= (length args) 2) (generate (car args)  (cadr args)))))
                        
        ((eq? msg 'vocab) vocab)))
 

    
    ; generate k random tokens of text subject to training distribution
    (define (generate k start-tok) 
      ; generate list of tokens
      (define (generate-helper k start)
        ; select one token for a given (n-1)-gram
        (define (rand-tok toks)
          (let*
              ; FIXME: this is slow
              ((keys (hash-keys freqs))
               (context (filter (lambda (k) (and (= (length k) n)
                                                 (equal? toks (get-n k (- n 1)))))
                                keys))
               (candidates  (map  last                              context))
               (frequencies (map (lambda (toks) (hash-ref ht toks)) context)))
            (car (sample (discrete-dist candidates frequencies) 1))))
        
        (if (= k 0)
            '()
            (let*
                ((next-tok (rand-tok start))
                 (next-ngram (append (cdr start) (list next-tok))))
              ; End of sentence?
              (if (equal? next-tok "</s>")
                  (list "</s>")
                  (cons next-tok
                        (generate-helper (- k 1) next-ngram))))))
      (append start-tok (generate-helper k start-tok)))
      
    
    
    ; Compute probability of a sentence
    (define (probability sent delta)
      (define (prob-helper s)
        (if (< (length s) n)
            0.0   ; log space  -> log(1) = 0
            (let
                ((count (hash-ref freqs (get-n s    n   ) epsilon1))
                 (total (hash-ref freqs (get-n s (- n 1)) epsilon2)))
              (let
                  ; smoothing
                  ((numer (+ count delta))
                   (denom (+ total (* delta
                                      (length (set->list vocab))))))                  
                   ;(print (list count delta))
                   ;(newline)
                   ;(print (list total (length (set->list vocab))))
                   ;(newline)
                   (lg* (fl (log (/ numer denom))) ; probability of ngram
                      (prob-helper (cdr s)))))))   ; probability of the rest
      (if (< (length sent) n)
          0 ; ERROR
          (let
              ; If VERY small, then return 0 
              ((retVal (expt 2.718281828459045 (prob-helper sent))))
            (if (< retVal (expt 10 -10))
                0
                retVal))))
    

    
    ; read the ngrams from the file
    (define (build-model-helper line)
      (if (eq? line eof)
          obj-interface
          (begin 
            ; read line into vocabulary
            (update-vocabulary (string-split line " "))
            
            ; count frquencies of n- and (n-1)-grams
            (count-sentence (string-split line " ")    n   ) 
            (count-sentence (string-split line " ") (- n 1)) 

            ; goto next line
            (build-model-helper (read-line my-in-port)))))


    ; Count frequencies of one line
    ; Buzzword: Map/Reduce
    (define (count-sentence line k)
      ; Group list of tokens into ngrams
      ; ex. (foldr collect '() '(a b c d e))  => '( (a b c) (b c d) (c d e) )
      (define (collect a result)
        (cond
          ((null? result) 
           (list (list a)))
          ((< (length (car result)) k)
           (list (cons a (car result))))
          (else
           (cons (cons a (get-n (car result) (- k 1)))
                 result))))
      
      ; Take an input ngram and increase the global count of it by 1
      (define (prob-ngram ngram)
        (begin
          (let
              ((freq (hash-ref freqs ngram 0)))
            (hash-set! freqs ngram (+ 1 freq))) ; increase gobal count
          0))
        
      (map prob-ngram (foldr collect '() line)))
    
    
    ; Update vocabulary of model
    (define (update-vocabulary s)
      (if (null? s)
          0
          (begin
            (cond
              ((and (not (equal? (car s) "<s>"))
                    (not (equal? (car s) "</s>")))
                 (set-add! vocab (car s))))
            (update-vocabulary (cdr s)))))
    
    
    ; get first n elements of list
    ; recursive process
    (define (get-n lst k)
      (if (or (= k 0) (null? lst))
          '()
          (cons (car lst)
                (get-n (cdr lst) (- k 1)))))


    ; Kick off line-by-line reader
    (build-model-helper (read-line my-in-port))))

  
  
;(define (main)

  ; define n
  (define n 2)
  
  ; ngram model
  (define freqs (build-ngram-model n))
  
  ; evaluate model
  (print (freqs 'prob "<s> Cher read a book </s>" 0))  ; unsmoothed
  (newline)
  (print (freqs 'prob "<s> Cher read a book </s>" 1))  ;   smoothed
  
;)



; call main
;(main)


; get first n elements of list
; recursive process
(define (get-n lst k)
  (if (or (= k 0) (null? lst))
      '()
      (cons (car lst)
            (get-n (cdr lst) (- k 1)))))


; work for generating random token
(define ht (freqs 'hash))
(define keys (hash-keys ht))

(define context (filter (lambda (k) (and (= (length k) n)
                                          (equal? '("read") (get-n k (- n 1)))))
                         keys))

(define candidates  (map  last                              context))
(define frequencies (map (lambda (toks) (hash-ref ht toks)) context))

(define d (discrete-dist candidates frequencies))

(length (filter (lambda (t) (equal? t "a")) (sample d 10000)))
