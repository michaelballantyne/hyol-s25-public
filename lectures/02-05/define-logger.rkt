#lang racket

(provide define-logger*)

(require (for-syntax syntax/parse racket/syntax))

(define (make-log-fn logger level)
  (lambda (msg)
    (log-message logger level msg)))

;; (define-logger* <name:id>)
;;
;; Defines a logger `name-logger`, and functions `log-name-info` and `log-name-error`
;; that each accept a message.
;;
;; Example:
#;(define-logger* request)
;; Expands to:
#;(begin
    (define request-logger (make-logger 'name))
    (define log-request-info (make-log-fn request-logger 'info))
    (define log-request-error (make-log-fn request-logger 'error)))
(define-syntax define-logger*
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id)
       (define/syntax-parse name-logger (format-id #'name "~a-logger" #'name))
       (define/syntax-parse log-name-info (format-id #'name "log-~a-info" #'name))
       (define/syntax-parse log-name-error (format-id #'name "log-~a-error" #'name))
       #'(begin
           (define name-logger (make-logger 'name))
           (define log-name-info (make-log-fn name-logger 'info))
           (define log-name-error (make-log-fn name-logger 'error)))])))

(begin-for-syntax
  (define (format-id ctx fstr . vals)
    (datum->syntax
     ctx
     (string->symbol
      (apply format fstr vals)))))