#lang scheme/base

; Start
(define (visit-doctor name)
    (print (list 'hello name))
    (print '(what seems to be the trouble?))
    (doctor-driver-loop name)
)

; Main loop
(define (doctor-driver-loop name)
    (newline)
    (print '**)
    (let ((user-response (read)))
        (cond
            ((equal? user-response '(goodbye))
                (print (list 'goodbye name))
                (print '(see you next week)))
            (else
                (print (reply user-response))
                (doctor-driver-loop name)
            )
        )
    )
)

; Reply to visitor
(define (reply user-response)
    (cond
        ((fifty-fifty) (append (qualifier) (change-person user-response)))
        (else (hedge))
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
   (many-replace '((i you) (me you) (am are) (my your)) phrase)
)

; Replace patterns to new values.
; replacement-pairs = ((pat1 val1) (pat2 val2) ...)
(define (many-replace replacement-pairs lst)
    (cond
        ((null? replacement-pairs) lst)
        (else
            (let ((pat-rep (car replacement-pairs)))
                (replace (car pat-rep)
                         (cadr pat-rep)
                         (many-replace (cdr replacement-pairs) lst)
                )
            )
        )
    )
)

; Replace every entry of pattern in lst
(define (replace pattern replacement lst)
    (cond
        ((null? lst) '())
        ((equal? (car lst) pattern)
            (cons replacement
                  (replace pattern replacement (cdr lst))
            )
        )
        (else
            (cons (car lst)
                  (replace pattern replacement (cdr lst))
            )
        )
    )
)

