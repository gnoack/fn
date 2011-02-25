
(defun concatenate-string (&rest strings)
  (apply #'concatenate 'string strings))

(defun join-strings (&rest list)
  #!+sb-doc
  "Joins a list of strings using a space sign."
  (format nil "~{~a~^ ~}" list))

(defun translate (input)
  #!+sb-doc
  "Translates an input program into a C-readable form."
  (cond ((eq nil input) "NIL")
	((listp input) (translate-list input))
	((numberp input) (translate-int input))
	((stringp input) (translate-string input))
	((symbolp input) (translate-symbol input))
	((characterp input) (translate-char input))
	(t (string input))))

(defun translate-list (list)
  (format nil "LIST(~{~a~^, ~})"
	  (mapcar #'translate list)))

(defun translate-int (int)
  (format nil "make_smallint(~aL)" int))

(defun translate-string (str)
  (format nil "LIST(make_symbol(\"list\"), ~{~a~^, ~})"
	  (map 'list #'translate-char str)))

(defun translate-char (ch)
  (format nil "make_char('~a')" ch))

(defun translate-symbol (sym)
  (let ((s (string-downcase (string sym))))
    (cond ((string= "nil" s) "NIL")
	  (t (format nil "make_symbol(\"~a\")" s)))))

(defun c-include (basename)
  (format nil "#include \"~a.h\"~%" basename))

(defun c-vardecl (basename value)
  (format nil "oop ~a_decls() { return ~a; }~%~%"
	  basename value))

;; Converts the fn-file with the given basename into a
;; c-file.
(defun convert (basename testp)
  (if testp
      (let* ((file-basename (format nil "~a-test" basename))
	     (var-basename (format nil "~a_test" basename))
	     (suffix (format nil "void ~a_tests() {~%  run_lisp_tests(~a_decls(), ~a_decls());~%}~%"
			     basename basename var-basename)))
	(actual-convert file-basename
			var-basename
			(concatenate 'string
				     (c-include "tests")
				     (c-include basename))
			suffix))
      (actual-convert basename basename "" "")))

(defun actual-convert (file-basename var-basename extra-includes suffix)
  (format t " * Converting ~a.fn…~%" file-basename)
  (with-open-file (out
		   (format nil "~a.c" file-basename)
		   :direction :output
		   :if-exists :supersede)
    (with-open-file (in (format nil "~a.fn" file-basename))
      (format out
	      "~a~%~a~a~a~a~%~a~a~%"
	      (c-include file-basename)
	      (c-include "cons")
	      (c-include "value")
	      (c-include "strings")
	      extra-includes
	      (c-vardecl var-basename
			 (translate (read in)))
	      suffix))))

;; (convert "utils" "")

(dolist (basename (cdr sb-ext:*posix-argv*))
  (format t "Processing ~a…~%" basename)
  (convert basename nil)
  (convert basename t))

(quit :unix-status 0)

