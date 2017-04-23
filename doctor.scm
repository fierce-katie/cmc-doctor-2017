#lang scheme/base

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
    (let ((strat (choose-strat answers)))
        (cond
            ((equal? strat 'qualifier)
                (append (qualifier) (change-person user-response))
            )
            ((equal? strat 'earlier)
                (append '(earlier you said that)
                         (change-person (pick-random answers))
                )
            )
            (else (hedge))
        )
    )
)

; #t or #f with equal probability
(define (fifty-fifty) (= (random 2) 0))

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

; Randomly chosen hedge
(define (hedge)
    (pick-random '((please go on)
                   (many people have the same sorts of feelings)
                   (many of my patients have told me the same thing)
                   (you are not the only one who feels like this)
                   (i see your problem)
                   (i think you need to tell me more about it)
                   (i can help you if you give more details)
                   (you can feel like this because of many different reasons)
                   (shall we change the topic)
                   (please continue)
                   (i see it really disturbs you)
                   (please tell me more about it))
    )
)

; Pick random element from list
(define (pick-random lst)
  (list-ref lst (random (length lst)))
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

; Choose answer generation strategy
(define (choose-strat answers)
    (if (null? answers)
        (pick-random '(qualifier hedge))
        (pick-random '(qualifier hedge earlier))
     )
)


