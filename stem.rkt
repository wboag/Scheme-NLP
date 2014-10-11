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
    
    
    ; Step 3
    (field (step3-suffixes '(("ICATE" "IC")
                             ("ATIVE" "")
                             ("ALIZE" "AL")
                             ("ICITI" "IC")
                             ("ICAL"  "IC")
                             ("FUL"   "")
                             ("NESS"  ""))))
    
    ; Step 4
    (field (step4-suffixes '(("AL"    "")
                             ("ANCE"  "")
                             ("ENCE"  "")
                             ("ER"    "")
                             ("IC"    "")
                             ("ABLE"  "")
                             ("IBLE"  "")
                             ("ANT"   "")
                             ("EMENT" "")
                             ("MENT"  "")
                             ("ENT"   "")
                             ("ION"   "")
                             ("OU"    "")
                             ("ISM"   "")
                             ("ATE"   "")
                             ("ITI"   "")
                             ("OUS"   "")
                             ("IVE"   "")
                             ("IZE"   ""))))
                             
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public  Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    (define/public (stem w)
      (begin
        (set! successful-1b #f)    ; rest global field
        (string-downcase (step4
                          (step3
                           (step2 
                            (step1 (string-upcase w))))))))

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
    (define/private (step1 str)
      (step1c (step1b (step1a str))))

    
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
            (sses(string-append (cadr sses) "SS"))
            (ies (string-append (cadr ies)  "I" ))
            (else str                             ))))

      ; SS   -> SS                         caress    ->  caress
      ; S    ->                            cats      ->  cat
      (define (step1a-2 str)
        (let
            ((ss   (regexp-match #rx"^(.*)SS$"   str))
             (s    (regexp-match #rx"^(.*)S$"     str)))
          (cond
            (ss  (string-append (cadr ss)   "SS"))
            (s                  (cadr s)         )
            (else str                              ))))

      (step1a-2 (step1a-1 w)))
    
    
    ;Step 1b
    (define/private (step1b w)

      (set! successful-1b #f)
      
      ;
      ;(m>0) EED -> EE                    feed      ->  feed
      ;                                   agreed    ->  agree
      ;(*v*) ED  ->                       plastered ->  plaster
      ;                                   bled      ->  bled
      ;(*v*) ING ->                       motoring  ->  motor
      ;                                   sing      ->  sing
      (define (step1b-main str)
        (let
            ((eed   (regexp-match #rx"^(.*)EED$" str))
             (ed    (regexp-match #rx"^(.*)ED$"  str))
             (ing   (regexp-match #rx"^(.*)ING$" str)))
          (cond
            (eed (if (> (get-m (cadr eed)) 0)
                     (string-append (cadr eed) "EE")
                     str))
            (ed  (if (contains-vowel? (cadr ed))
                     (begin
                       (set! successful-1b #t)
                       (cadr ed))
                     str))
            (ing (if (contains-vowel? (cadr ing))
                     (begin
                       (set! successful-1b #t)
                       (cadr ing))
                     str))
            (else str))))
        
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
        (define (step1b-extra str)
          (if successful-1b
              (let
                  ((at    (regexp-match #rx"^(.*)AT$"   str))
                   (bl    (regexp-match #rx"^(.*)BL$"   str))
                   (iz    (regexp-match #rx"^(.*)IZ$"   str))
                   (final (string-ref str (- (string-length str) 1))))
                (cond
                  (at (string-append (cadr at) "ATE"))
                  (bl (string-append (cadr bl) "BLE"))
                  (iz (string-append (cadr iz) "IZE"))
                  ((and (ends-double-c? str)
                        (not (equal? final #\L))
                        (not (equal? final #\S))
                        (not (equal? final #\Z)))
                   (list->string (reverse (cdr (reverse (string->list str))))))
                  ((and (ends-cvc? str)
                        (= (get-m str) 1)
                   (string-append str "E")))
                  (else str)))
              str))
        
        (let
            ((stemmed (step1b-main w)))
          (if successful-1b
              (step1b-extra stemmed)
              stemmed)))      

    ; step 1c
    ;
    ;(*v*) Y -> I                    happy        ->  happi
    ;                                sky          ->  sky
    (define/private (step1c w)
      (let
          ((y   (regexp-match #rx"^(.*)Y$" w)))
        (if (or (not y)
                (not (contains-vowel? (cadr y))))
            w
            (string-append (cadr y) "i"))))

    

    ; Step 2
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
    (define/private (step2 w)
      
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
        
    
    
    
    ; Step 3
    ;
    ;(m>0) ICATE ->  IC              triplicate     ->  triplic
    ;(m>0) ATIVE ->                  formative      ->  form
    ;(m>0) ALIZE ->  AL              formalize      ->  formal
    ;(m>0) ICITI ->  IC              electriciti    ->  electric
    ;(m>0) ICAL  ->  IC              electrical     ->  electric
    ;(m>0) FUL   ->                  hopeful        ->  hope
    ;(m>0) NESS  ->                  goodness       ->  good
    (define/private (step3 w)
      
      ; Iterate over step2-suffixes list to find right substitution
      (define (step3-helper s suffixes)
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
                  (step3-helper s (cdr suffixes))))))
      
      (step3-helper w step3-suffixes))
    
    
    
    ; Step 4
    ;
    ;(m>1) AL    ->                  revival        ->  reviv
    ;(m>1) ANCE  ->                  allowance      ->  allow
    ;(m>1) ENCE  ->                  inference      ->  infer
    ;(m>1) ER    ->                  airliner       ->  airlin
    ;(m>1) IC    ->                  gyroscopic     ->  gyroscop
    ;(m>1) ABLE  ->                  adjustable     ->  adjust
    ;(m>1) IBLE  ->                  defensible     ->  defens
    ;(m>1) ANT   ->                  irritant       ->  irrit
    ;(m>1) EMENT ->                  replacement    ->  replac
    ;(m>1) MENT  ->                  adjustment     ->  adjust
    ;(m>1) ENT   ->                  dependent      ->  depend
    ;(m>1 and (*S or *T)) ION ->     adoption       ->  adopt
    ;(m>1) OU    ->                  homologou      ->  homolog
    ;(m>1) ISM   ->                  communism      ->  commun
    ;(m>1) ATE   ->                  activate       ->  activ
    ;(m>1) ITI   ->                  angulariti     ->  angular
    ;(m>1) OUS   ->                  homologous     ->  homolog
    ;;(m>1) IVE   ->                  effective      ->  effect
    ;(m>1) IZE   ->                  bowdlerize     ->  bowdler
    (define/private (step4 w)
      
      ; Iterate over step2-suffixes list to find right substitution
      (define (step4-helper s suffixes)
        (if (null? suffixes)
            s
            (let
                ((match (regexp-match 
                        (pregexp (string-append "^(.*)"
                                                (caar suffixes)
                                                "$"))
                        s)))
              (if (and match                    ; match found
                       (> (get-m (car match)) 1))      ; new stem must have m>0
                  (let
                      ((stem (cadr match)))
                    (if (and (equal? (caar suffixes) "ION")
                             (not (equal? (string-last stem) #\S))
                             (not (equal? (string-last stem) #\T)))
                        (step4-helper s (cdr suffixes))
                        (string-append stem (cadar suffixes))))
                  (step4-helper s (cdr suffixes))))))
            
      (step4-helper w step4-suffixes))
    
    

    ; Step 5
    (define/public (step5 w)
      
      ;Step 5a
      ;(m>1) E     ->                  probate        ->  probat
      ;                                rate           ->  rate
      ;(m=1 and not *o) E ->           cease          ->  ceas
      (define (step5a str)
        (let
            ((e (regexp-match #rx"^(.*)E$" str)))
          (if (and
               e
               (or
                (> (get-m (cadr e)) 1) 
                (and (= (get-m (cadr e)) 1)
                     (not (ends-cvc? (cadr e))))))
               (cadr e)
               str)))
          
      ;Step 5b
      ;(m > 1 and *d and *L) -> single letter
      ;                                controll       ->  control
      ;                                roll           ->  roll
      (define (step5b str)
        (if (and (> (get-m str) 1)
                 (ends-double-c? str)
                 (equal? (string-last str) #\L))
            (list->string (reverse (cdr (reverse (string->list str)))))
            str))
      
      (step5b (step5a w)))
    
              
    ; General purpoe helper function
    (define/private (string-last s)
      (string-ref s (sub1 (string-length s))))

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
  (send porter step5 w))


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

;(define words '("HAPPY"
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
 

(define words '("PROBATE"
                "RATE"
                "CEASE"
                "CONTROLL"
                "ROLL"))

(display words)
(newline)
(display (map porter-stem words))