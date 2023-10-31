;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-module (srfi srfi-248)
  #:use-module ((rnrs exceptions) #:select ((guard . rnrs:guard)))
  #:export (raise-continuable with-unwind-handler guard)
  #:replace (raise)
  #:re-export (with-exception-handler => else))

(define (with-unwind-handler handler thunk)
  (define tag (make-prompt-tag 'handler))
  (let f ([thunk thunk])
    (call-with-prompt tag
        (lambda ()
          (with-exception-handler
              (lambda (obj)
                (abort-to-prompt tag obj))
            thunk))
      (lambda (k obj)
        (handler obj
                 (lambda arg*
                   (f (lambda () (apply k arg*)))))))))

(define (raise obj)
  (raise-exception obj))

(define (raise-continuable obj)
  (raise-exception obj #:continuable? #t))

(define-syntax guard
  (lambda (stx)
    (syntax-case stx (else)
      [(guard (c k cl ... [else e1 ... e2])
         b1 ... b2)
       (and (identifier? #'c)
            (identifier? #'k))
       #'(with-unwind-handler
             (lambda (c k)
               (cond cl ... [else e1 ... e2]))
           (lambda ()
             b1 ... b2))]
      [(guard (c k cl1 ... cl2)
         b1 ... b2)
       (and (identifier? #'c)
            (identifier? #'k))
       #'(with-unwind-handler
             (lambda (obj k)
               (let ([c obj])
                 (cond cl1 ... cl2
                       [else (call-with-values (lambda () (raise-continuable obj))
                               k)])))
           (lambda ()
             b1 ... b2))]
      [(guard (c cl ... [else e1 ... e2])
         b1 ... b2)
       (identifier? #'c)
       #'((call/cc
           (lambda (k)
             (with-exception-handler
                 (lambda (c)
                   (k (lambda ()
                        (cond cl ... [else e1 ... e2]))))
               (lambda ()
                 (call-with-values (lambda () b1 ... b2)
                   (case-lambda
                     [(val) (lambda () val)]
                     [val* (lambda () (apply values val*))])))))))]
      [(guard (c cl1 ... cl2)
         b1 ... b2)
       (identifier? #'c)
       #'((call/cc
           (lambda (kouter)
             (with-exception-handler
                 (lambda (obj)
                   ((call/cc
                     (lambda (khandler)
                       (kouter
                        (lambda ()
                          (let ([c obj])
                            (cond
                             cl1 ... cl2
                             [else
                              (khandler
                               (lambda ()
                                 (raise-continuable obj)))]))))))))
               (lambda ()
                 (call-with-values (lambda () b1 ... b2)
                   (case-lambda
                     [(val) (lambda () val)]
                     [val* (lambda () (apply values val*))])))))))])))
