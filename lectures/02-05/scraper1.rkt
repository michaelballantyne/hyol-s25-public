#lang racket

(require racket/logging net/url)

(define request-logger (make-logger))

;; (-> Any), LogLevel -> (values Any String)
(define (run-with-log thunk level)
  (let ([log (open-output-string)])
    (define res
      (with-logging-to-port log
        thunk
        #:logger request-logger
        level))
    (values res (get-output-string log))))

(define (scrape url)
  ;; Log at info level:
  (log-message request-logger 'info (format "downloading url ~a" url))
  (with-handlers ([exn:fail?
                   (lambda (ex)
                     ;; Log at error level:
                     (log-message request-logger 'error
                                  (format "could not connect to url ~a" url)))])
    (port->string (get-pure-port (string->url url)))))

(define (task)
  (scrape "https://mballantyne.ne/hyl"))

(run-with-log task 'info)