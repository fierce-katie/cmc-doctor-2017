#lang scheme/base

; STRATEGY PREDICATES

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

; List of strategy predicates and matching functions
(define strategies
    (list
        (cons length-pred lenght-func)
        (cons earlier-pred earlier-func)
        (cons keywords-pred keywords-func)
    )
)

(define pred car)
(define func cdr)

; KEYWORDS STRATEGY

; Keywords
(define keywords '(depressed suicide mother father parents son daughter work job study university))

; List keywords found in user response
(define (get-keywords user-response)
    (cond
        ((null? user-response) '())
        ((member (car user-response) keywords)
            (cons (car user-response) (get-keywords (cdr user-response)))
        )
        (else (get-keywords (cdr user-response)))
    )
)

; Use random keyphrase for one of keywords
(define (use-keywords keys)
    (let ((chosen-key (pick-random keys)))
        (cond
            ((member chosen-key '(depressed suicide))
                (pick-random '((when you feel depressed go out for an ice-cream)
                               (depression is a disease that can be treated))
                )
            )
            ((member chosen-key '(mother father parents son daughter))
                (append
                    (pick-random '((tell me more about your)
                                   (why do you feel that way about your)
                                   (have you always been treated like that by your)
                                   (can you improve the relationship with your)
                                   (have you talked about it with your)
                                   (will you talk about it with your))
                    )
                    (list chosen-key)
                )
            )
            ((member chosen-key '(work job))
                (append
                    (pick-random '((tell me more about your)
                                   (have you always felt like this about your)
                                   (are you stressed because of your)
                                   (would you like to change your))
                    )
                    (list chosen-key)
                )
            )
            (else (pick-random '((what do you study)
                                 (do you like the place where you study)
                                 (do you like the subjects you study)
                                 (do you want to give up your studies)
                                 (do you have friends there))
                  )
            )
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

; STRATEGY CHOICE

; Get list of all strategy functions that can be applied
(define (choose-strats user-response answers)
    (append  (list hedge-func qualifier-func)
             (filter-preds strategies user-response answers)
    )
)

; Filter strategy functions that can be applied
(define (filter-preds strats user-response answers)
    (define (go lst acc)
        (cond
            ((null? lst) acc)
            (((pred (car lst)) user-response answers)
                (go (cdr lst) (cons (func (car lst)) acc))
            )
            (else (go (cdr lst) acc))
        )
    )
    (go strats '())
)

; UTILS

; #t or #f with equal probability
(define (fifty-fifty) (= (random 2) 0))

; Pick random element from list
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; MAIN

; Start
(define (visit-doctor name)
    (cond
        ((equal? name 'suppertime) (print '(time to go home!)))
        (else
            (print (list 'hello name))
            (print '(what seems to be the trouble?))
            (doctor-driver-loop name '())
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
(define (doctor-driver-loop name answers)
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
                (print (reply user-response answers))
                (doctor-driver-loop name (cons user-response answers))
            )
        )
    )
)

; Reply to visitor
(define (reply user-response answers)
    (let*
        ((strats (choose-strats user-response answers))
         (strat-func (pick-random strats)))
        (display strats)
        (newline)
        (strat-func user-response answers)
    )
)

