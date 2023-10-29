(import (srfi :248))
(import (srfi :64))
(import (rnrs io simple))
(import (rnrs conditions))

(test-begin "SRFI 248")

(test-equal 1
  (with-exception-handler/k
      (lambda (obj k)
        obj)
    (lambda ()
      (raise 1))))

(test-equal 2
  (with-exception-handler/k
      (lambda (obj k)
        obj)
    (lambda ()
      (raise-continuable 2))))

(test-equal 3
  (with-exception-handler/k
      (lambda (obj k)
        (+ 1 obj))
    (lambda ()
      (with-exception-handler/k
          (lambda (obj k)
            (raise (+ 1 obj)))
        (lambda ()
          (raise 1))))))

(test-equal 4
  (with-exception-handler/k
      (lambda (obj k)
        (and (non-continuable-violation? obj)
             4))
    (lambda ()
      (with-exception-handler/k
          (lambda (obj k)
            (k 2))
        (lambda ()
          (raise 1))))))

(test-equal 5
  (with-exception-handler/k
      (lambda (obj k)
        (+ 2 (k (+ 3 obj))))
    (lambda ()
      (+ 1 (raise-continuable -1)))))

(test-equal 6
  (with-exception-handler/k
      (lambda (obj k)
        (+ 2 (k (k (+ 3 obj)))))
    (lambda ()
      (+ 1 (raise-continuable -1)))))

(test-equal 7
  (guard (c [(number? c) (+ 1 c)])
    (raise 6)))

(test-equal 8
  (guard (c [(symbol? c) #f]
            [else (+ 2 c)])
    (raise 6)))

(test-equal 9
  (guard (c [else c])
    (guard (c [(symbol? c) #f])
      (raise 9))))

(define (call-with-state proc init)
  (define-condition-type &get &condition
    make-get-condition get-condition?)
  (define-condition-type &put &condition
    make-put-condition put-condition?
    (value condition-value))
  (define (get) (raise-continuable (make-get-condition)))
  (define (put val) (raise-continuable (make-put-condition val)))
  ((guard (obj k
               [(get-condition? obj)
                (lambda (s)
                  ((k s) s))]
               [(put-condition? obj)
                (lambda (s)
                  ((k) (condition-value obj)))])
     (call-with-values (lambda () (proc get put))
       (lambda arg*
         (lambda (s)
           (apply values arg*)))))
   init))

(test-equal 10
  (call-with-state
   (lambda (get put)
     (+ 1 (get)))
   9))

(test-equal 11
  (call-with-state
   (lambda (get put)
     (put (+ 1 (get)))
     (+ 1 (get)))
   9))

(test-equal 12
  (call-with-state
   (lambda (get put)
     (call/cc
      (lambda (c)
        (put 13)
        (c)))
     (get))
   12))

(test-equal (list 13 '(exit enter exit enter exit enter))
  (let ([trace* '()])
    (guard (obj k
                [else
                 (let ([v (k (k obj))])
                   (list v trace*))])
      (dynamic-wind
        (lambda ()
          (set! trace* (cons 'enter trace*)))
        (lambda ()
          (+ 1 (raise-continuable 11)))
        (lambda ()
          (set! trace* (cons 'exit trace*)))))))

(define make-coroutine-generator
  (lambda (proc)
    (define-condition-type &yield &condition
      make-yield-condition yield-condition?
      (value condition-value))
    (define yield
      (lambda (val)
        (raise-continuable (make-yield-condition val))))
    (define thunk
      (lambda ()
        (guard (c k
                  [(yield-condition? c)
                   (set! thunk k)
                   (condition-value c)])
          (proc yield)
          (eof-object))))
    (lambda ()
      (thunk))))

(define generator->list
  (lambda (g)
    (let f ()
      (let ([x (g)])
        (if (eof-object? x)
            '()
            (cons x (f)))))))

(test-equal '(1 2)
  (let ([g (make-coroutine-generator
            (lambda (yield)
              (yield 1)
              (yield 2)))])
    (generator->list g)))

(define for-each->fold
  (lambda (for-each)
    (define-condition-type &yield &condition
      make-yield-condition yield-condition?
      (value condition-value))
    (lambda (proc nil)
      ((guard (c k
                 [(yield-condition? c)
                  (lambda (s)
                    ((k) (proc s (condition-value c))))])
         (for-each
           (lambda (x)
             (raise-continuable (make-yield-condition x))))
         values)
       nil))))

(define (xcons x y) (cons y x))

(test-equal '(3 2 1)
  ((for-each->fold
    (lambda (f)
      (for-each f '(1 2 3))))
   xcons '()))

(test-end)

;; Local Variables:
;; eval: (put 'with-exception-handler/k 'scheme-indent-function 1)
;; End:
