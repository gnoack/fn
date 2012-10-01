#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 format))
;; (define-module (translate)
;;   #:use-module (ice-9 format)
;;   #:export (main))

(define (sanitize str)
  (string-map (lambda (ch)
                (if (or (char-alphabetic? ch)
                        (char-numeric? ch))
                    ch
                    #\_))
              str))

(define (translate input)
  (cond ((null? input)
         "NIL")
        ((list? input)
         (format #f "LIST(~{~a~^, ~})" (map translate input)))
        ((number? input)
         (format #f "make_smallint(~aL)" input))
        ((string? input)
         (format #f "make_string(~s)" input))
        ((char? input)
         (format #f "make_char('~a')"
                 (cond ((char=? input #\\) "\\\\")
                       ((char=? input #\') "\\'")
                       ((char=? input #\") "\\\"")
                       ((char=? input #\tab) "\\t")
                       ((char=? input #\newline) "\\n")
                       (else input))))
        ((symbol? input)
         (if (eq? 'nil input)
             "NIL"
             (format #f "make_symbol(\"~s\")" input)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formatting                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c-include basename)
  (format #f "#include \"~a.h\"~%" basename))

(define (c-vardecl basename value)
  (format #f "oop ~a_decls() { return ~a; }~%~%"
          (sanitize basename) value))

(define (make-test-formatter basename)
  (let ((test-basename (format #f "~a-test" basename))
        (var-basename (format #f "~a_test" basename)))
    (lambda (outstream cdecls)
      (format outstream
              "~a~%~%~a~%~a~a~a~%~a~%~avoid ~a_tests() {~%  run_lisp_tests(~a_decls());~%}~%"
              "// Auto-generated."
              (c-include test-basename)
              (c-include "cons")
              (c-include "value")
              (c-include "strings")
              (c-include "tests")
              (c-vardecl var-basename
                         cdecls)
              (sanitize basename)
              (sanitize var-basename)))))

(define (make-prod-formatter basename)
  (lambda (outstream cdecls)
    (format outstream
            "~a~%~a~%~a~a~%~a~%~a"
            "// Auto-generated."
            (c-include basename)
            (c-include "cons")
            (c-include "value")
            (c-include "strings")
            (c-vardecl basename
                       cdecls))))

(define (actual-convert formatter file-basename)
  (format #t " * Converting ~a.fn... (Scheme)~%" file-basename)
  (call-with-output-file (format #f "~a.c" file-basename)
    (lambda (out)
      (call-with-input-file (format #f "~a.fn" file-basename)
        (lambda (in)
          (formatter out (translate (read in))))))))

(define (convert-prod basename)
  (actual-convert (make-prod-formatter basename)
                  basename))

(define (strip-suffix str suffix)
  (if (string-suffix? suffix str)
      (substring str 0 (- (string-length str) (string-length suffix)))
      str))

(define (convert-test basename)
  (actual-convert (make-test-formatter (strip-suffix basename "-test"))
                  basename))

(define (main args)
  (for-each
   (lambda (basename)
     (if (string-suffix? "-test" basename)
         (convert-test basename)
         (convert-prod basename)))
   (cdr args)))
