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




(define ngram-model
  (class object%
  
    (super-new)

    ; FIXME - Does not work with n=1
    (init-field n)
    
    (field (freqs       (make-hash)))   ; Buzzword: Hash Table
    (field (vocab     (mutable-set)))
    (field (epsilon1  (expt 10 -20)))
    (field (epsilon2  (expt 10 -10)))
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public  Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    ; train: [train-file]
    ; ex. (send this-model train "data/greet.txt")
    (define/public (train . train-file)
      ; OOV token for predicting on unseen tokens
      (set-add! vocab "OOV")
      
      ; parse arguments
      (cond
        ((= (length train-file) 0)
         (build-model-helper (open-input-file (string->path "data/greet.txt"))))
        ((= (length train-file) 1)
         (build-model-helper (open-input-file (string->path (car train-file)))))
        (else
         (error "Too many arguments given to train"))))


    ; Compute probability of a sentence
    (define/public (probability sent . args)
      (priv-probability (tokenize sent) args))

        
    ; generate k random tokens of text subject to training distribution
    (define/public (generate k start-tok)
      (if (= (length start-tok) (- n 1))
          (append start-tok (priv-generate k start-tok))
          (error "start-token argument for generate is wrong length")))
     
    
    ; Gives access to ngram counts
    (define/public (get-frequency phrase)
      (let
          ((k (length phrase)))
        (if (or (= k (sub1 n)) (= k n))
            (hash-ref freqs phrase 0)
            (error (string-append "Cannot look up frequency of "
                                  (number->string k)
                                  "-gram")))))

    ; Debugging purposes - give access to data members
    ;(define/public (get-freqs) freqs)
    ;(define/public (get-vocab) vocab)
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    ; Tokenize sentence
    (define/private (tokenize sent)
      (string-split sent " "))

    
    
    ; Compute probability of list of tokens
    (define/private (priv-probability toks args)
      
      ; Iterate over sentence
      (define (helper-prob s)
        (if (< (length s) n)
            0.0   ; log space  -> log(1) = 0
            (let
                ((count (hash-ref freqs (get-n s    n   ) epsilon1))
                 (total (hash-ref freqs (get-n s (- n 1)) epsilon2)))
              (let*
                  ; smoothing
                  ((smoothed (smoothing args count total)))
                (lg* (fl (log smoothed))         ; probability of ngram
                     (helper-prob (cdr s)))))))  ; probability of the rest

      ; Replace unseen tokens with OOV token
      (define (helper-OOV-convert tok) 
        (if (set-member? vocab tok) tok "OOV"))
      
      (if (< (length toks) n)
          (error "toks argument for priv-probability is wrong length")
          (let*
              ((oov-replaced (map helper-OOV-convert toks))
               (sent-with-tags (append '("<s>") oov-replaced '("</s>")))
               (retVal (expt 2.718281828459045 (helper-prob sent-with-tags))))

            ; If VERY small, then return 0 
            (if (< retVal (expt 10 -10))
                0
                retVal))))
    
    
    
    ; read the ngrams from the file
    (define/private (build-model-helper my-in-port)
      (let
          ((line (read-line my-in-port)))
        (if (eq? line eof)
            void
            (begin 
              ; read line into vocabulary
              (update-vocabulary (tokenize line))
              
              ; count frquencies of n- and (n-1)-grams
              (let
                  ((toks (append '("<s>")
                                 (tokenize line)
                                 '("</s>"))))
                (count-sentence toks    n   ) 
                (count-sentence toks (- n 1)))
              ; goto next line
              (build-model-helper my-in-port)))))
    

    
    ; Update vocabulary of model
    (define/private (update-vocabulary s)
      (if (null? s)
          0
          (begin
            (cond
              ((and (not (equal? (car s) "<s>"))
                    (not (equal? (car s) "</s>")))
               (set-add! vocab (car s))))
            (update-vocabulary (cdr s)))))
    
    
    
    ; Count frequencies of one line
    ; Buzzword: Map/Reduce
    (define/private (count-sentence line k)

      ; Group list of tokens into ngrams
      ; ex. (foldr collect '() '(a b c d e))  => '( (a b c) (b c d) (c d e) )
      (define (helper-collect a result)
        (cond
          ((null? result) 
           (list (list a)))
          ((< (length (car result)) k)
           (list (cons a (car result))))
          (else
           (cons (cons a (get-n (car result) (- k 1)))
                 result))))
      
      ; Take an input ngram and increase the global count of it by 1
      (define (helper-prob-ngram ngram)
        (begin
          (let
              ((freq (hash-ref freqs ngram 0)))
            (hash-set! freqs ngram (+ 1 freq))) ; increase global count
          void))

      ; map and reduce over line to count ngram frequencies
      (map helper-prob-ngram (foldr helper-collect '() line)))
    
    
    
    ; get first n elements of list
    (define/private (get-n lst k)
      (if (or (= k 0) (null? lst))
          '()
          (cons (car lst)
                (get-n (cdr lst) (- k 1)))));;
    
    

    ; Smoothing 
    (define/private (smoothing args count total)
      (cond
        ; no smoothing
        ((null? args) 
         (/ count total))
        
        ; additive (Laplace) smoothing
        ((equal? (car args) 'additive)
         (let
             ((delta (cadr args)))
           (/ (+ count delta)
              (+ total (* delta (length (set->list vocab)))))))
        ))
      
      
      
    ; generate list of tokens
    (define/private (priv-generate k start)
      
      ; select one token for a given (n-1)-gram
      (define (helper-rand-tok toks)
        (let*
            ; FIXME: this is slow
            ((keys (hash-keys freqs))
             (context (filter (lambda (k) (and (= (length k) n)
                                               (equal? toks (get-n k (- n 1)))))
                              keys))
             (candidates  (map  last                              context))
             (frequencies (map (lambda (toks) (hash-ref freqs toks)) context)))
          ; Build and sample distribution
          (car (sample (discrete-dist candidates frequencies) 1))))
      
      (if (= k 0)
          '()
          (let*
              ((next-tok (helper-rand-tok start))
               (next-ngram (append (cdr start) (list next-tok))))
            ; End of sentence?
            (if (equal? next-tok "</s>")
                (list "</s>")
                (cons next-tok
                      (priv-generate (- k 1) next-ngram))))))
    
))
   


; Instantiate model
(define model-a (new ngram-model (n 2)))
(define model-b (new ngram-model (n 1)))

; Train model on input data
(send model-a train "data/greet.txt")

; Predict probabiity of a sentence
(display (send model-a probability "Cher read a book"))
(newline)
(display (send model-a probability "Cher read a book" 'additive 1))
(newline)

; Generate a random sequence of text
(display (send model-a generate 10 '("<s>")))
(newline)
