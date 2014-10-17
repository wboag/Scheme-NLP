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

(require "tokenizer.rkt")
(require "parse-args.rkt")


; Make ngram model importable
(provide ngram-model)



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
    ;
    ; TODO
    ;      1) Add argument to give list of list of tokens
    ;      2) Iterate over tokens & update vocab IN train
    ;
    (define/public (train . args)
      ; OOV token for predicting on unseen tokens
      (set-add! vocab "OOV")
      
      (let
          ((toks (parse-train-toks (cons 'tags args))))

        ; read the ngrams from the file
        ;(define/private (build-model-helper my-in-port)
        ;  (let
        ;      ((line (read-line my-in-port)))
        ;    (if (eq? line eof)
        ;        void
        ;        (begin 
        ;          ; read line into vocabulary
        ;          (update-vocabulary (tokenize line))
        ;           
        ;           ; count frquencies of n- and (n-1)-grams
        ;           (let
        ;              ((toks(tokenize line 'tags)))
        ;            (count-sentence toks    n   ) 
        ;            (count-sentence toks (- n 1)))
        ;          ; goto next line
        ;          (build-model-helper my-in-port)))))        
        
        ; Update vocabulary with all toks
        (map (lambda (t)
               (cond
                 ((and (not (equal? t "<s>"))
                      (not (equal? t "</s>")))
                 (set-add! vocab t))))
             (foldr (lambda (this result)
                      (append this result))
                    '()
                    toks))
  
        ; Count frequency of n- and (n-1)-grams
        (define (count-grams k ll-of-toks)
          (map (lambda (sent)
                 (count-sentence sent k))
               ll-of-toks))        
        (count-grams (- n 1) toks)
        (count-grams    n    toks)

        void))
    

    ; probability: <n-gram|(n-1)-gram> ['smooth 'additive <number>]
    ;
    ; Purpose: Compute the probability of a sequence of tokens.
    ;
    ; Optional Aruments:
    ;
    ;   smooth - Enables smoothing of counts
    ;
    ;        additive - Laplace Smoothing. Takes a number (delta) to add to
    ;                       each n-gram frequency.
    ;                   * Can be called on n-grams and (n-1)-grams.
    ;                   * NOTE: the delta passed will count each n-gram, 
    ;                           meaning that the smoothed frequency of an 
    ;                           (n-1)-gram is increased by  |V| * delta, where 
    ;                           V is the size of the vocabulary.
    ;
    ;
    ; TODO - Change so that adding <s> and </s> tags is optional & defaults to off
    ; 
    (define/public (probability sent . args)
      (priv-probability (sentence->sentence-tagged-tokens sent)
                        args))

    
    ; generate: <int> <(n-1)-gram>
    ;
    ; Purpose: Generate k random tokens of text using training distribution.
    ;
    ; Arguments:
    ;
    ;   k - How many random tokens to generate.
    ;
    ;   start-toks - Beginning (n-1)-gram to start phrase generation.
    ;
    ; TODO - Have generator begin new sentence after </s> rather than quit.
    ;
    (define/public (generate k start-toks)
      (if (= (length start-toks) (- n 1))
          (append start-toks (priv-generate k start-toks))
          (error "start-tokens argument for generate is wrong length")))
     

    ; get-frequency: <phrase> ['smooth 'additive <number>]
    ;
    ; Purpose: Gives access to ngram counts
    ;
    ; Optional Aruments:
    ;
    ;   smooth - Enables smoothing of counts
    ;
    ;        additive - Laplace Smoothing. Takes a number (delta) to add to
    ;                       each n-gram frequency.
    ;                   * Can be called on n-grams and (n-1)-grams.
    ;                   * NOTE: the delta passed will count each n-gram, 
    ;                           meaning that the smoothed frequency of an 
    ;                           (n-1)-gram is increased by  |V| * delta, where 
    ;                           V is the size of the vocabulary.
    ;
    (define/public (get-frequency phrase . args)
      (let
          ((k (length phrase)))
        (if (or (= k (sub1 n)) (= k n))
            (smoothed phrase args)
            (error (string-append "Cannot look up frequency of "
                                  (number->string k)
                                  "-gram")))))
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    ; Compute probability of list of tokens
    (define/private (priv-probability toks args)
      
      ; Iterate over sentence
      (define (helper-prob s)
        (if (< (length s) n)
            0.0   ; log space  -> log(1) = 0
            (let
                ((count (smoothed (get-n s    n   ) args))
                 (total (smoothed (get-n s (- n 1)) args)))
              (lg* (fl (log (/ count total)))    ; probability of ngram
                   (helper-prob (cdr s))))))    ; probability of the rest
      
      
      ; Replace unseen tokens with OOV token
      (define (helper-OOV-convert tok) 
        (if (or (set-member? vocab tok)
                (equal? tok "<s>")
                (equal? tok "</s>"))
            tok 
            "OOV"))
      
      (if (< (length toks) n)
          (error "toks argument for priv-probability is wrong length")
          (let*
              ((oov-replaced (map helper-OOV-convert toks))
               (retVal (expt 2.718281828459045 (helper-prob oov-replaced))))
            
            ; If VERY small, then return 0 
            (if (< retVal (expt 10 -10))
                0
                retVal))))

        
    
    ; Count frequencies of one line
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
                (get-n (cdr lst) (- k 1)))))
    

    ; Smoothing
    (define/private (smoothed phrase args)     
      (if (and (not (= (length phrase) (- n 1)))
               (not (= (length phrase)    n  )))
          (error "Cannot get frequency of non n/(n-1)-gram")
          (let*
              ((true-count (hash-ref freqs phrase (if (= (length phrase) n)
                                                      epsilon1
                                                      epsilon2))))
            (cond
              ; no smoothing
              ((not (memq 'smooth args))
               true-count)
              
              ; additive (Laplace) smoothing
              ((memq 'additive args)
               (let
                   ((delta (cadr (memq 'additive args))))
                 (if (= (length phrase) n)
                     (+ true-count delta)                  ;  n   -gram
                     (+ true-count (* delta                ; (n-1)-gram
                                      (length (set->list vocab))))))) 
              
              (else (error "Other smoothing methods are not yet supported"))))))
      

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
   

