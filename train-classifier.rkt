**** guess.rkt *****************************************************************
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(require "animals.rkt")

;;***********CONSTANTS FOR TESTING**************
(define test1
  (list
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))
(define test2
  (list
   (list 'goose 'large 'swims 'angry)
   (list 'crow 'medium 'large)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'squirrel 'small 'angry)))
(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))
;;**********************************************




;;**********
;;Part A
;;**********


;;(collect-attributes example): consumes a list of examples and produces a list of all the attributes
;;                              contained in that example

;;Examples:
(check-expect (collect-attributes test1) (list 'swims 'large 'angry 'flies 'medium))
(check-expect (collect-attributes empty) empty)
(check-expect (collect-attributes (list)) empty)

;;collect-attributes: (listof Example) --> (listof Sym)
(define (collect-attributes example)
  (local [(define (single-list lst acc)
            (cond [(empty? lst) acc]
                  [else (single-list (rest lst) (cons (first lst) acc))]))
          (define (complete-list nest-lst)
            (cond [(empty? nest-lst) empty]
                  [else (append (single-list (rest (first nest-lst)) empty) (collect-attributes (rest nest-lst)))]))
          (define comp (complete-list example))
          (define (final-list lst)
            (cond [(empty? lst) empty]
                  [(member? (first lst) (rest lst)) (final-list (rest lst))]
                  [else (cons (first lst) (final-list (rest lst)))]))]
    (final-list comp)))

;;Tests:
(check-expect (collect-attributes seen) (list 'small 'swims 'large 'angry 'flies 'medium))
(check-expect (collect-attributes (list (list 'goose 'large 'swims 'flies 'angry))) (list 'angry 'flies 'swims 'large))
(check-expect (collect-attributes (list (list 'goose))) empty)



;;**********
;;Part B
;;**********


;;(split-examples example symbol): consumes a list of examples and symbol and produces list with 2 terms. The first  
;;                                 term being a list of examples containing that symbol (positive) and the second being the list
;;                                 of examples not containing that symbol (negative).


;;Examples:
(check-expect (split-examples seen 'goose) (list
                                            (list (list 'goose 'large 'swims 'flies 'angry)
                                                  (list 'goose 'large 'swims 'flies 'angry))
                                            (list (list 'squirrel 'small 'angry)
                                                  (list 'crow 'medium 'flies 'angry))))
(check-expect (split-examples seen 'small) (list
                                            (list (list 'squirrel 'small 'angry))
                                            (list (list 'goose 'large 'swims 'flies 'angry)
                                                  (list 'goose 'large 'swims 'flies 'angry)
                                                  (list 'crow 'medium 'flies 'angry))))


;;split-examples: (Listof Example) Sym --> (list (Listof Example) (Listof Example))                                               
(define (split-examples example symbol)
  (local [(define (found nest-list sym)
            (cond [(empty? nest-list) empty]
                  [(member? sym (first nest-list)) (cons (first nest-list) (found (rest nest-list) sym))]
                  [else (found (rest nest-list) sym)]))
          (define (not-found nest-list sym)
            (cond [(empty? nest-list) empty]
                  [(member? sym (first nest-list)) (not-found (rest nest-list) sym)]
                  [else (cons (first nest-list) (not-found (rest nest-list) sym))]))]
    (list (found example symbol) (not-found example symbol))))


;;Tests:
(check-expect (split-examples empty 'crow) (list empty empty))
(check-expect (split-examples seen 'emu) (list
                                          empty
                                          (list (list 'squirrel 'small 'angry)
                                                (list 'goose 'large 'swims 'flies 'angry)
                                                (list 'goose 'large 'swims 'flies 'angry)
                                                (list 'crow 'medium 'flies 'angry))))
(check-expect (split-examples (list (list 'goose 'large 'swims 'flies 'angry)
                                    (list 'goose 'large 'swims 'flies 'angry)) 'goose)
              (list (list (list 'goose 'large 'swims 'flies 'angry)
                          (list 'goose 'large 'swims 'flies 'angry))
                    empty))




;;**********
;;Part C
;;**********


;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.


;;(histogram example): consumes a list of examples and produces a list of attribute/count pairs, with each
;;                     pair indicating how many times that attribute appears in the examples.

;;Examples:
(check-expect (histogram seen) (list (list 'small 1) (list 'swims 2) (list 'large 2) (list 'angry 4) (list 'flies 3) (list 'medium 1)))
(check-expect (histogram test1) (list (list 'swims 1) (list 'large 1) (list 'angry 2) (list 'flies 2) (list 'medium 1)))

;;histogram: (Listof Example) --> Histogram
(define (histogram example)
  (local [(define (count lst sym num) 
            (cond [(empty? lst) num]
                  [(symbol=? sym (first lst)) (count (rest lst) sym (add1 num))]
                  [else (count (rest lst) sym num)]))
          (define (total nest-list sym-list)
            (cond [(empty? nest-list) 0]
                  [else (+ (count (first nest-list) (first sym-list) 0) (total (rest nest-list) sym-list))]))
          (define (final-list nest-list sym-list)
            (cond [(empty? sym-list) empty]
                  [else (cons (list (first sym-list) (total nest-list sym-list)) (final-list nest-list (rest sym-list)))]))]
    (final-list example (collect-attributes example))))

;;Tests:
(check-expect (histogram empty) empty)
(check-expect (histogram (list (list 'goose) (list 'crow))) empty)
(check-expect (histogram (list (list 'goose 'large) (list 'crow))) (list (list 'large 1)))




;;**********
;;Part D
;;**********


;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.


;;(augment-histogram histogram attributes total): consumes a histogram, list of attributes and a num and :-
;;                                                i) adds any missing attributes not present in Histogram with a count of 0
;;                                                ii) adds another number to the list of (list Sym Num) in Histogram equal to
;;                                                    the difference of total and count of each attribute.


;;Examples:
(check-expect (augment-histogram (list (list 'a 100) (list 'c 50)) (list 'a 'b 'c) 200) (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect (augment-histogram empty (list 'x 'y) 10) (list (list 'x 0 10) (list 'y 0 10)))

;;augment-histogram: Histogram (Listof Sym) Nat --> AH
;;requires:
;;  total is greater than or equal to any count of histogram
(define (augment-histogram histogram attributes total)
  (local [(define (super-member? sym nest-list)
            (cond [(empty? nest-list) false]
                  [(member? sym (first nest-list)) true]
                  [else (super-member? sym (rest nest-list))]))
          (define (new-augment hist attributes-list acc)
            (cond [(empty? attributes-list) (append hist acc)]
                  [(super-member? (first attributes-list) hist) (new-augment hist (rest attributes-list) acc)]
                  [else (new-augment hist (rest attributes-list) (cons (list (first attributes-list) 0) acc))]))
          (define (final-list nest-list total)
            (cond [(empty? nest-list) empty]
                  [else (cons (list (first (first nest-list)) (second (first nest-list))
                                    (- total (second (first nest-list))))
                              (final-list (rest nest-list) total))]))
          (define final (final-list (new-augment histogram attributes empty) total))
          (define (sorted-list att AH-list const)
            (cond [(empty? att) empty]
                  [(member? (first att) (first AH-list)) (cons (first AH-list) (sorted-list (rest att) const const))]
                  [else (sorted-list att (rest AH-list) const)]))]
    (sorted-list attributes final final)))

;;Tests:
(check-expect (augment-histogram (list (list 'a 100) (list 'c 50)) empty 200) empty)
(check-expect (augment-histogram empty empty 200) empty)
(check-expect (augment-histogram (list (list 'z 10) (list 'y 20)) (list 'x 'y 'z) 50) (list (list 'x 0 50) (list 'y 20 30) (list 'z 10 40)))



;;**********
;;Part E
;;**********


;;(entropy AH1 AH2): consumes 2 Augmented Histograms and computes their entropy by the given formula.

;;Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669)) 0.566 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361)) 0.582 0.001)

;;entropy: AH AH --> Num
(define (entropy AH1 AH2)
  (local [(define (probability m n)
            (cond [(= (+ m n) 0) 0.5]
                  [else (/ m (+ m n))]))
          (define (find-e p)
            (cond [(= p 0) 0]
                  [else (* -1 p (log p 2))]))
          (define a (second AH1))
          (define b (second AH2))
          (define c (third AH1))
          (define d (third AH2))]
    (+ (* (probability (+ a b) (+ c d)) (+ (find-e (probability a b)) (find-e (probability b a))))
       (* (probability (+ c d) (+ a b)) (+ (find-e (probability c d)) (find-e (probability d c)))))))

;;Tests:
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0 0.001)
(check-within (entropy (list 'a 0 0) (list 'b 0 0)) 1 0.001)
(check-within (entropy (list 'a 100 0) (list 'b 100 0)) 1 0.001)




;;**********
;;Part F
;;**********


;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.


;;(entropy-attributes positive negative): consumes two Augmented Histograms and computes the entropy of each
;;                                        attribute, producing a list of attribute/entropy pairs.

;;Examples:
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600))) (list
                                                                (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
                                                                (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
                                                                (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)) 0.001)

;;entropy-attributes: AH AH --> EAL
;;requires:
;;  positive and negative AH have the same attributes in the same order
(define (entropy-attributes positive negative)
  (local [(define (ent-att-list AH1-list AH2-list)
            (cond [(or (empty? AH1-list) (empty? AH2-list)) empty]
                  [else (cons (list (first (first AH1-list)) (entropy (first AH1-list) (first AH2-list)))
                              (ent-att-list (rest AH1-list) (rest AH2-list)))]))]
    (ent-att-list positive negative)))

;;Tests:
(check-within (entropy-attributes empty empty) empty 0.001)
(check-within (entropy-attributes
               (list (list 'large 126 59))
               (list (list 'large 146 669))) (list (list 'large #i0.5663948489857853)) 0.001)




;;**********
;;Part G
;;**********


;;(best-attribute entropies): consumes an EAL and produces the attribute having minimum attribute

;;Example:
(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)

;;best-attribute: EAL --> (Anyof Sym or empty)
(define (best-attribute entropies)
  (local [(define (min-entropy entropy-list acc)
            (cond [(empty? entropy-list) (first acc)]
                  [(< (second (first entropy-list)) (second acc)) (min-entropy (rest entropy-list) (first entropy-list))]
                  [else (min-entropy (rest entropy-list) acc)]))]
    (cond [(empty? entropies) empty]                   
          [else (min-entropy entropies (first entropies))])))

;;Tests:
(check-expect (best-attribute (list)) empty)
(check-expect (best-attribute (list (list 'angry #i0.5447688190492))) 'angry)





;;**********
;;Part H
;;**********


;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)


;;(build-dt examples label): consumes a list of examples and a symbol and produces a DT based on that symbol(label).

;;Examples:
(check-expect (build-dt seen 'goose) (list 'swims true false))
(check-expect (build-dt (random-animals 1000) 'emu) false)

;;build-dt: (Listof Example) Sym --> DT
(define (build-dt examples label)
  (local [(define attributes (collect-attributes examples))
          (define positive (first (split-examples examples label)))
          (define negative (second (split-examples examples label)))
          (define AH-positive (augment-histogram (histogram positive) attributes (length positive)))
          (define AH-negative (augment-histogram (histogram negative) attributes (length negative)))
          (define root-attribute (best-attribute (entropy-attributes AH-positive AH-negative)))
          (define (rmv-att-single-lst lst att)
            (cond [(empty? lst) empty]
                  [(symbol=? att (first lst)) (rmv-att-single-lst (rest lst) att)]
                  [else (cons (first lst) (rmv-att-single-lst (rest lst) att))]))
          (define (remove-root-attribute nest-list att)
            (cond [(empty? nest-list) empty]
                  [else (cons (rmv-att-single-lst (first nest-list) att) (remove-root-attribute (rest nest-list) att))]))]
    (cond [(empty? positive) false]
          [(empty? negative) true]
          [(empty? attributes) (cond [(> (length positive) (length negative)) true]
                                     [else false])]
          [(equal? (build-dt (remove-root-attribute (first (split-examples examples root-attribute)) root-attribute) label)
                   (build-dt (second (split-examples examples root-attribute)) label))
           (build-dt (second (split-examples examples root-attribute)) label)]
          [else (list root-attribute (build-dt (remove-root-attribute (first (split-examples examples root-attribute)) root-attribute) label)
                      (build-dt (second (split-examples examples root-attribute)) label))])))


;;Tests:
(check-expect (build-dt empty 'goose) false)
(check-expect (build-dt (list (list 'goose) (list 'crow)) 'goose) false)
(check-expect (build-dt (list (list 'goose 'large)) 'goose) true)




;;**********
;;Part I
;;**********


;;(train-classifier examples label): generates a decision tree from the examples, and then produces a predicate
;;                                   that consumes a list of attributes and produces a decision (ie a Bool).

;;Examples:
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)

;;train-classifier: (Listof Example) Sym --> (Listof Sym -> Bool)
(define (train-classifier examples label)
  (local [(define DT (build-dt examples label))
          (define (make-predicate DT attributes)
            (cond [(equal? DT false) false]
                  [(equal? DT true) true]
                  [(member? (first DT) attributes) (make-predicate (second DT) attributes)]
                  [else (make-predicate (third DT) attributes)]))
          (define (attribute-intake attribute)
            (make-predicate DT attribute))]
    attribute-intake))

;;Tests:
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(check-expect (crow? (list 'angry 'flies 'medium)) true)


;;*****************CONSTANTS FOR (I)*********************************
(define goose? (train-classifier (random-animals 1000) 'goose))
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(define crow? (train-classifier (random-animals 1000) 'crow))
;;*******************************************************************





;;**********
;;Part J
;;**********


;;(performance classifier? examples label): classifies on the basis of sensitivity and specificty of the label.

;;Examples:
(check-expect (performance goose? seen 'goose) (list 'goose 100 100))

;;performance: ((Listof Sym) -> Bool) (Listof Examples) Stm -->  (list Sym Nat Nat)
(define (performance classifier? examples label)
  (local [(define (new-examples lst)
            (cond [(empty? lst) empty]
                  [else (cons (rest (first lst)) (new-examples (rest lst)))]))
          (define positive (new-examples (first (split-examples examples label))))
          (define negative (new-examples (second (split-examples examples label))))
          (define (positive-examples lst num) 
            (cond [(empty? lst) num]
                  [(classifier? (first lst)) (positive-examples (rest lst) (add1 num))]
                  [else (positive-examples (rest lst) num)]))
          (define (negative-examples lst num)
            (cond [(empty? lst) num]
                  [(classifier? (first lst)) (negative-examples (rest lst) num)]
                  [else (negative-examples (rest lst) (add1 num))]))
          (define sensitivity (cond [(empty? positive) 0]
                                    [else (round (* 100 (/ (positive-examples positive 0) (length positive))))]))
          (define specificity (cond [(empty? negative) 0]
                                    [else (round (* 100 (/ (negative-examples negative 0) (length negative))))]))]
    (list label sensitivity specificity)))

;;Tests:
(check-expect (performance goose? (list) 'goose) (list 'goose 0 0))
