#lang scheme/base

; CONTROL STRATEGIES

(define (random-weights user-response answers _ans-strats)
    (let*
        ((strats (choose-strats user-response answers))
         (strat-func (pick-weighted strats)))
        (cons strat-func (strat-func user-response answers))
    )
)

(define (cond-weights user-response answers _ans-strats)
    (let
        ((strat-func (choose-cond-weight strategies user-response answers)))
        (cons strat-func (strat-func user-response answers))
    )
)

(define (hist-strats user-response answers ans-strats)
    (if (null? ans-strats)
        (let*
            ((strats (choose-strats user-response answers))
             (strat-func (car (pick-random strats))))
             (cons strat-func (strat-func user-response answers))
        )
        (let*
            ((strats (choose-strats user-response answers))
             (excl (get-most-often ans-strats))
             (strat-func (pick-random-excl strats excl)))
            (cons strat-func (strat-func user-response answers))
        )
    )
)

(define control-strats (list random-weights cond-weights hist-strats))

; CHECKING HISTORY CONTROL STRATEGY

; Pick random list element, excluding certain value
(define (pick-random-excl lst excl)
    (display (list "excl" excl))
    (let ((x (car (pick-random lst))))
        (display (list "random" x))
        (if (equal? x excl)
            (pick-random-excl lst excl)
            x
        )
    )
)

; Get strat that has been used more than 30% in previous answers
; strats = (<item> . <count>) and is not empty
(define (get-most-often strats)
    (let*
        ((m (max-total strats))
         (total (cdr m))
         (f (caar m))
         (v (cdar m)))
        (display (list "total" total))
        (display (list "max" (car m)))
        (if (> v (* total 0.33))
            f
            #f
        )
    )
)

; Calculate total number of items and the maximum
(define (max-total lst)
    (define (update-max m1 m2)
        (if (> (cdr m1) (cdr m2))
            m1
            m2
        )
    )
    (define (go l total m)
        (if (null? l)
            (cons m total)
            (go (cdr l) (+ total (cdar l)) (update-max m (car l)))
        )
    )
    (go lst 0 '(#f . 0))
)

; COND WITH WEIGHTS CONTROL STRATEGY

(define (choose-cond-weight strats user-response answers)
    (cond
        ((null? strats) hedge-func) ; this shouldn't happen
        (((pred (car strats)) user-response answers) (func (car strats)))
        (else (choose-cond-weight (cdr strats) user-response answers))
    )
)

; RANDOM-WEIGHTS CONTROL STRATEGY

; Get list of all strategy functions that can be applied
(define (choose-strats user-response answers)
    (filter-preds strategies user-response answers)
)

; Filter strategies that can be applied
(define (filter-preds strats user-response answers)
    (define (go lst acc)
        (cond
            ((null? lst) acc)
            (((pred (car lst)) user-response answers)
                (go (cdr lst) (cons (cdr (car lst)) acc))
            )
            (else (go (cdr lst) acc))
        )
    )
    (go strats '())
)

; Pick random strategy using weights.
; strats = ((func weitght) ...)
(define (pick-weighted strats)
    (define (go cur aim lst)
        (cond
            ((null? (cdr lst))
                (car lst)
            )
            ((>= (+ cur (cadar lst)) aim)
                (car lst)
            )
            (else
                (go (+ cur (cadar lst)) aim (cdr lst))
            )
         )
    )
    (let*
        ((total (foldl + 0 (map cadr strats)))
         (rnd (+ 1 (random total))))
        ;(display rnd) (newline)
        (car (go 0 rnd strats))
    )
)
; STRATEGY PREDICATES

(define (hedge-pred _user-response _ans-strats) #t)
(define qualifier-pred hedge-pred)

; Check if user answer is too short
(define (length-pred user-response _answers)
    (< (length user-response) 3)
)

; Check if user has already said something
(define (earlier-pred _user-response answers)
    (pair? answers)
)

; Check if user answer contains keywords
(define (keywords-pred user-response _answers)
    (pair? (get-keywords user-response))
)

; STRATEGY FUNCTIONS

; Ask to say more if the answer is too short
(define (lenght-func _user-response _answers)
    (pick-random '((could you say more)
                   (i would like to know more about it)
                   (i think you need to tell me more about it)
                   (i can help you if you give more details)
                   (please tell me more about it))
    )
)

; Randomly choose one of the previous answers
(define (earlier-func _user-response answers)
    (append '(earlier you said that)
            (change-person (pick-random answers))
    )
)

; Say key phrase that matches a random keyword from user answer
(define (keywords-func user-response _answers)
    (use-keywords (get-keywords user-response))
)

; Say some general phrase
(define (hedge-func _user-response _answers) (hedge))

; Say some general phrase referring to current response
(define (qualifier-func user-response _answers)
    (append (qualifier) (change-person user-response))
)

; List of strategy predicates and matching functions,
; sorted by weight
(define strategies
    (list
        (list keywords-pred keywords-func 5)
        (list length-pred lenght-func 4)
        (list earlier-pred earlier-func 3)
        (list qualifier-pred qualifier-func 2)
        (list hedge-pred hedge-func 1)
    )
)

; "Getters" for strategy components
(define pred car)
(define func cadr)
(define weight caddr)

; KEYWORDS STRATEGY

; Key phrases ((<keywords> <phrases>) ...)
(define key-phrases
    '(
         ((depressed suicidal) ; depression
             ((when you feel depressed go out for an ice-cream)
              (depression is a disease that can be treated))
         )
         ((mother father parents son daughter) ;family
             ((tell me more about your *)
              (can you improve the relationship with your *)
              (have you talked about it with your *)
              (does your * treat everyone else like that)
              (your * seems to trouble you)
              (will you talk about it with your *))
         )
         ((work job) ; work
             ((tell me more about your *)
              (are you stressed because of your *)
              (is your * like you imagined it would be)
              (what * would you prefer instead)
              (would you like to change your *))
         )
         ((study university) ; study
             ((what do you study)
              (do you like the place where you study)
              (do you like the subjects you study)
              (do you want to give up your studies)
              (do you have friends there))
         )
         ((ugly creepy fat) ; appearence
             ((thinking you are * may lead to depression)
              (your personality is much more important than your look)
              (have anyone told you that you are * when you were a child)
              (dont let anyone say you are *))
         )
     )
)

; List keywords found in user response
(define (get-keywords user-response)
    (define (go resp keywords)
        (cond
            ((null? resp) '())
            ((member (car resp) keywords)
                (cons (car resp) (go (cdr resp) keywords))
            )
            (else (go (cdr resp) keywords))
        )
    )
    (go user-response (foldl append '() (map car key-phrases)))
)

; Use random keyphrase for one of keywords
(define (use-keywords keys)
    (let ((chosen-key (pick-random keys)))
        (replace-all chosen-key
            (pick-random (get-key-phrases chosen-key key-phrases))
        )
    )
)

; Get all key phrases that match given keyword
(define (get-key-phrases keyword phrases)
    (define (go phs acc)
        (cond
            ((null? phs) acc)
            ((member keyword (caar phs))
                (go (cdr phs) (append (cadar phs) acc))
            )
            (else
                (go (cdr phs) acc)
            )
        )
    )
    (go phrases '())
)

; Replace all * to <new>
(define (replace-all new lst)
    (cond
        ((null? lst) lst)
        ((equal? (car lst) '*)
            (cons new (replace-all new (cdr lst)))
        )
        (else
            (cons (car lst) (replace-all new (cdr lst)))
        )
    )
)

; QUALIFIER STRATEGY

; Randomly chosen qualifier
(define (qualifier)
    (pick-random '((you seem to think)
                   (you feel that)
                   (why do you believe)
                   (why do you say)
                   (do you know why)
                   (how often do you think)
                   (when did you start to feel)
                   (we can discuss why you feel that)
                   (is there anyone who made you feel that))
    )
)

; Change person in user's phrase
(define (change-person phrase)
    (many-replace
        '((i you) (me you) (am are) (my your) (are am) (you i) (your my))
        phrase
    )
)

; Replace patterns to new values.
; replacement-pairs = ((pat1 val1) (pat2 val2) ...)
(define (many-replace replacement-pairs lst)
    (map (lambda (x) (replace replacement-pairs x)) lst)
)

; Replace element with first matching replacement
(define (replace pairs elem)
    (cond
        ((null? pairs) elem)
        ((equal? (caar pairs) elem) (cadar pairs))
        (else (replace (cdr pairs) elem))
    )
)

; HEDGE STRATEGY

; Randomly chosen hedge
(define (hedge)
    (pick-random '((please go on)
                   (many people have the same sorts of feelings)
                   (many of my patients have told me the same thing)
                   (you are not the only one who feels like this)
                   (i see your problem)
                   (you can feel like this because of many different reasons)
                   (shall we change the topic)
                   (please continue)
                   (i see it really disturbs you)
                   (please tell me more about it))
    )
)

; UTILS

; #t or #f with equal probability
(define (fifty-fifty) (= (random 2) 0))

; Pick random element from list
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; Store items in list (<item> . <count>)
(define (count-strat new prev)
    (cond
        ((null? prev) (list (cons new 1)))
        ((equal? new (caar prev))
            (cons (cons new (+ 1 (cdar prev))) (cdr prev))
        )
        (else (cons (car prev) (count-strat new (cdr prev))))
    )
)

; MAIN

; Start
(define (visit-doctor name)
    (cond
        ((equal? name 'suppertime) (print '(time to go home!)))
        (else
            (print (list 'hello name))
            (print '(what seems to be the trouble?))
            (doctor-driver-loop name '() '() (pick-random control-strats))
        )
    )
)

; Ask patient's name
(define (ask-name)
    (print '(next!))
    (print '(who are you?))
    (newline)
    (print '**)
    (car (read))
)

; Main loop
(define (doctor-driver-loop name answers ans-strats ctl-strat)
    (print ctl-strat)
    (newline)
    (print '**)
    (let ((user-response (read)))
        (cond
            ((equal? user-response '(goodbye))
                (print (list 'goodbye name))
                (print '(see you next week))
                (newline)
                (visit-doctor (ask-name))
            )
            (else
                (let*
                    ((rep (reply ctl-strat user-response answers ans-strats))
                     (prev-strat (car rep))
                     (phrase (cdr rep)))
                    (print prev-strat)
                    (print ans-strats)
                    (print phrase)
                    (doctor-driver-loop name (cons user-response answers) (count-strat prev-strat ans-strats) ctl-strat)
                )
            )
        )
    )
)

; Reply to visitor
(define (reply ctl-strat user-response answers ans-strats) (ctl-strat user-response answers ans-strats))

