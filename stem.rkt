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

    (field (successful-1b #f))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public  Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    (define/public (stem w)
      (begin
        (set! successful-1b #f)    ; rest global field
        (string-downcase (step2 (step1 (string-upcase w))))))

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    (define/private (step1 str)
      (step1b (step1a str)))

    
    ;Step 1a
    (define/private (step1a w)

      ; SSES -> SS                         caresses  ->  caress
      ; IES  -> I                          ponies    ->  poni
      ;                                    ties      ->  ti
      (define (step1a-1 str)
        (let
            ((sses (regexp-match #rx"^(.*)SSES$" str))
             (ies  (regexp-match #rx"^(.*)IES$"  str)))
          (cond
            (sses(string-append (second sses) "SS"))
            (ies (string-append (second ies)  "I" ))
            (else str                             ))))

      ; SS   -> SS                         caress    ->  caress
      ; S    ->                            cats      ->  cat
      (define (step1a-2 str)
        (let
            ((ss   (regexp-match #rx"^(.*)SS$"   str))
             (s    (regexp-match #rx"^(.*)S$"     str)))
          (cond
            (ss  (string-append (second ss)   "SS"))
            (s                  (second s)         )
            (else str                              ))))

      (step1a-2 (step1a-1 w)))
    
    
    ;Step 1b
    (define/private (step1b w)

      ;(m>0) EED -> EE                    feed      ->  feed
      ;                                   agreed    ->  agree
      (define (step1b-1 str)
        (let
            ((eed   (regexp-match #rx"^(.*)EED$"   str)))
          (if (or (not eed) 
                  (= (get-m str) 0))
              str
            (string-append (second eed) "EE"))))
            
      ;(*v*) ED  ->                       plastered ->  plaster
      ;                                   bled      ->  bled
      (define (step1b-2 str)
        (let
            ((eed   (regexp-match #rx"^(.*)ED$"   str)))
          (if (or (not eed)
                  (not (contains-vowel? 
                        (substring str 0 (- (string-length str) 2)))))
              str
              (begin 
                (set! successful-1b #t)
                (second eed)))))
   
      ;(*v*) ING ->                       motoring  ->  motor
      ;                                   sing      ->  sing
      (define (step1b-3 str)
        (let
            ((eed   (regexp-match #rx"^(.*)ING$"   str)))
          (if (or (not eed)
                  (not (contains-vowel? (substring str 
                                                   0 
                                                   (max 0 
                                                        (- (string-length str) 
                                                           3))))))
              str
              (begin
                (set! successful-1b #t)
                (second eed)))))
      
      ;  AT -> ATE                       conflat(ed)  ->  conflate
      ;  BL -> BLE                       troubl(ed)   ->  trouble
      ;  IZ -> IZE                       siz(ed)      ->  size
      ;(*d and not (*L or *S or *Z))
      ;   -> single letter
      ;                                hopp(ing)    ->  hop
      ;                                tann(ed)     ->  tan
      ;                                fall(ing)    ->  fall
      ;                                hiss(ing)    ->  hiss
      ;                                fizz(ed)     ->  fizz
      ;(m=1 and *o) -> E               fail(ing)    ->  fail
      ;                                fil(ing)     ->  file      
      (define (step1b-4 str)
        (if successful-1b
            (let
                ((at    (regexp-match #rx"^(.*)AT$"   str))
                 (bl    (regexp-match #rx"^(.*)BL$"   str))
                 (iz    (regexp-match #rx"^(.*)IZ$"   str))
                 (final (string-ref str (- (string-length str) 1))))
              (cond
                (at (string-append (second at) "ATE"))
                (bl (string-append (second bl) "BLE"))
                (iz (string-append (second iz) "IZE"))
                ((and (ends-double-c? str)
                      (not (equal? final #\L))
                      (not (equal? final #\S))
                      (not (equal? final #\Z)))
                 (list->string (reverse (cdr (reverse (string->list str))))))
                ((ends-cvc? str) (string-append str "E"))
                (else str)))
            str))
        
      ; Apply all sub-steps in step 1b
      (step1b-4 (step1b-3 (step1b-2 (step1b-1 w)))))

    
    ; Step 2
    (field (step2-suffixes '( ("ATIONAL" "ATE")
                              ("TIONAL"  "TION")
                              ("ENCI"    "ENCE")
                              ("ANCI"    "ANCE")
                              ("IZER"    "IZE")
                              ("ABLI"    "ABLE")
                              ("ALLI"    "AL")
                              ("ENTLI"   "ENT")
                              ("ELI"     "E")
                              ("OUSLI"   "OUS")
                              ("IZATION" "IZE")
                              ("ATION"   "ATE")
                              ("ATOR"    "ATE")
                              ("ALISM"   "AL")
                              ("IVENESS" "IVE")
                              ("FULNESS" "FUL")
                              ("OUSNESS" "OUS")
                              ("ALITI"   "AL")
                              ("IVITI"   "IVE")
                              ("BILITI"  "BLE"))))

    ; Step 2
    (define (step2 w)
      
      ; Iterate over step2-suffixes list to find right substitution
      (define (step2-helper s suffixes)
        (if (null? suffixes)
            s
            (let
                ((stem (regexp-match 
                        (pregexp (string-append "^(.*)"
                                                (caar suffixes)
                                                "$"))
                        s)))
              (if (and stem                        ; match found
                       (> (get-m (cadr stem)) 0))  ; new stem must have m>0
                  (string-append (cadr stem) (cadar suffixes))
                  (step2-helper s (cdr suffixes))))))
      
      (step2-helper w step2-suffixes))
        
    
    ; (m>0) ATIONAL ->  ATE           relational     ->  relate
    ; (m>0) TIONAL  ->  TION          conditional    ->  condition
    ;                                 rational       ->  rational
    ; (m>0) ENCI    ->  ENCE          valenci        ->  valence
    ; (m>0) ANCI    ->  ANCE          hesitanci      ->  hesitance
    ; (m>0) IZER    ->  IZE           digitizer      ->  digitize
    ; (m>0) ABLI    ->  ABLE          conformabli    ->  conformable
    ; (m>0) ALLI    ->  AL            radicalli      ->  radical
    ; (m>0) ENTLI   ->  ENT           differentli    ->  different
    ; (m>0) ELI     ->  E             vileli        - >  vile
    ; (m>0) OUSLI   ->  OUS           analogousli    ->  analogous
    ; (m>0) IZATION ->  IZE           vietnamization ->  vietnamize
    ; (m>0) ATION   ->  ATE           predication    ->  predicate
    ; (m>0) ATOR    ->  ATE           operator       ->  operate
    ; (m>0) ALISM   ->  AL            feudalism      ->  feudal
    ; (m>0) IVENESS ->  IVE           decisiveness   ->  decisive
    ; (m>0) FULNESS ->  FUL           hopefulness    ->  hopeful
    ; (m>0) OUSNESS ->  OUS           callousness    ->  callous
    ; (m>0) ALITI   ->  AL            formaliti      ->  formal
    ; (m>0) IVITI   ->  IVE           sensitiviti    ->  sensitive
    ; (m>0) BILITI  ->  BLE           sensibiliti    ->  sensible
    

    ; does input w contain any vowels?
    (define/private (contains-vowel? w)
      (ormap (lambda (i) (not (consonant? w i))) 
             (range 0 (string-length w))))
    
    
    ; Is character w[i] a consonant?
    (define/private (consonant? str ind)
      (not
       (regexp-match? #rx"((?i:(?<=.)(?i:a|e|i|o|u)|(?<=[^aeiou])y))"
                      (substring str 
                                 (max 0 (- ind 1)) 
                                 (+ 1 ind)))))

    
    ; Does character end with a repeated consonant?
    (define/private (ends-double-c? w)
      (regexp-match #px"^.*([^aeiouy])\\1$" w))


    ; Does the stem end with cvc, where the second c is not W, X or Y ?
    (define/private (ends-cvc? w)
      (let*
          ((n (string-length w))
           (c (char-upcase (string-ref w (- n 1)))))
        (and
         (> n 2)
         (     consonant? w (- n 1) )
         (not (consonant? w (- n 2)))
         (     consonant? w (- n 3) )
         (not (equal? c #\W))
         (not (equal? c #\X))
         (not (equal? c #\Y)))))

      
    ; Get "measure of string 
    (define/private (get-m s)
      ; Number of inner VC pairs => half of the length of the inner list
      (- (/ (length (CV-split s)) 2) 1))
    
    
    ; parition word into alternating list of consontant and vowel substrings
    ; Note: Always begins with consontant strng & ends with vowel string
    ;                        C   V   C    V   C    V   C   V   C   V
    ; ex. "alternating" => '("" "a" "lt" "e" "rn" "a" "t" "i" "ng" "")
    (define/private (CV-split str)
      
      ; "alterating" => '("" "a" "lternating")
      (define (get-leading-CV s)
        (let
            ((   C-regex "^((?i:y?[^aeiouy]*))"          )
             (   V-regex "((?i:(?:y[aeiou]*|[aeiou]*)))" )
             (rest-regex "(.*)"                          ))                
          (cdr (regexp-match (pregexp (string-append    C-regex
                                                        V-regex
                                                        rest-regex))
                             s))))
      
      ; iterative process to split string by repeatedly calling get-leading-CV
      (define (CV-split-helper s result)
        (if (= 0 (string-length s))
              result
            (let*
                ((C-V-rest (get-leading-CV s))
                 (C        (first  C-V-rest) )
                 (V        (second C-V-rest) )
                 (rest     (third  C-V-rest) ))
              (CV-split-helper rest (append result (list C V) )))))
      
      ; Break down str into C-Vs
      (CV-split-helper str '()))
          
))



; Instantiate object
(define porter (new porter-object))


; Mappable function
(define (porter-stem w)
  (send porter stem w))


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
;                "SING"))

;(define words '("CONFLATED"
;                "TROUBLED"
;                "SIZED"
;                "HOPPING"
;                "TANNED"
;                "FALLING"
;                "HISSING"
;                "FIZZED"
;                "FAILING"
;                "FILING"))

(define words '("RELATIONAL"
                "CONDITIONAL"
                "RATIONAL"
                "VALENCI"
                "HESITANCI"
                "DIGITIZER"
                "CONFORMABLI"
                "RADICALLI"
                "DIFFERENTLI"
                "VILELI"
                "ANALOGOUSLI"
                "VIETNAMIZATION"
                "PREDICATION"
                "OPERATOR"
                "FEUDALISM"
                "DECISIVENESS"
                "HOPEFULNESS"
                "CALLOUSNESS"
                "FORMALITI"
                "SENSITIVITI"
                "SENSIBILITI"))

(display words)
(newline)
(display (map porter-stem words))