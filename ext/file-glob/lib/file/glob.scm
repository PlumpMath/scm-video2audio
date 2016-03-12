(define-module (file glob)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex))

(define %strict-wild-card-slash 1)
(define %strict-leading-dot     1)

(define* (just x #:optional (judge? equal?))
  (lambda (other)
    (judge? x other)))

(define (one-of . args)
  (lambda (x)
    (any (just x) args)))

;; * -> .*
;; . -> \.
;;

(define (glob->regex-string glob-pattern)
  (let ((in-curlies 0)
        (escaping   0)
        (first-byte 1)
        (symbol-of  (one-of #\( #\. #\) #\| #\+ #\^ #\$ #\@ #\%)))
    (with-output-to-string
      (lambda ()
        (for-each (lambda (ch)
                    (if (> first-byte 0)
                        (begin
                          (if (> %strict-leading-dot 0)
                              (unless (equal? ch #\.)
                                      (display "(?=[^\\.])")))
                          (set! first-byte 0)))
                    (cond ((equal? ch #\/)
                           (set! first-byte 1))
                          ((symbol-of ch)
                           (display (string #\\ ch)))
                          ((equal? ch #\*)
                           (display (if (> escaping 0)
                                        "\\*"
                                        (if (> %strict-wild-card-slash 0)
                                            "[^/]*"
                                            ".*"))))
                          ((equal? ch #\?)
                           (display (if (> escaping 0)
                                        "\\?"
                                        (if (> %strict-wild-card-slash 0)
                                            "[^/]"
                                            "."))))
                          ((equal? ch #\{)
                           (display (if (> escaping 0)
                                        "\\{"
                                        "("))
                           (unless (> escaping 0)
                                   (set! in-curlies (+ in-curlies 1))))
                          ((and (equal? ch #\})
                                (> in-curlies 0))
                           (display (if (> escaping 0)
                                        "}"
                                        ")"))
                           (unless (> escaping 0)
                                   (set! in-curlies (- in-curlies 1))))
                          ((and (equal? ch #\,)
                                (> in-curlies 0))
                           (display (if (> escaping 0)
                                        ","
                                        "|")))
                          ((equal? ch "\\")
                           (if (> escaping 0)
                               (begin
                                 (display "\\\\")
                                 (set! escaping 0))
                               (set! escaping 1)))
                          (else
                           (display ch)
                           (set! escaping 0))))
                  (string->list glob-pattern))))))

(define-public (glob pattern)
  (let* ((current-dir (getcwd))
         (dirs        (scandir current-dir))
         (re          (make-regexp (glob->regex-string pattern))))
    (remove (lambda (dir)
              (string-match re dir))
            dirs)))
