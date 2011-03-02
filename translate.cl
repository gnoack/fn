
(defun concatenate-string (&rest strings)
  (apply #'concatenate 'string strings))

(defgeneric translate (input)
  (:documentation
   "Translates an input program into a C-readable form."))

;; (defun translate (input)
;;   #!+sb-doc
;;   "Translates an input program into a C-readable form."
;;   (cond ((eq nil input) "NIL")
;; 	((listp input) (translate-list input))
;; 	((numberp input) (translate-int input))
;; 	((stringp input) (translate-string input))
;; 	((symbolp input) (translate-symbol input))
;; 	((characterp input) (translate-char input))
;; 	(t (string input))))

(defmethod translate ((item (eql nil)))
  "NIL")

(defmethod translate ((l list))
  (format nil "LIST(~{~a~^, ~})"
	  (mapcar #'translate l)))

(defmethod translate ((int integer))
  (format nil "make_smallint(~aL)" int))

(defmethod translate ((str string))
  (format nil "LIST(make_symbol(\"list\"), ~{~a~^, ~})"
	  (map 'list #'translate str)))

(defmethod translate ((ch character))
  (format nil "make_char('~a')" ch))

(defmethod translate ((sym symbol))
  (let ((s (string-downcase (string sym))))
    (cond ((string= "nil" s) "NIL")
	  (t (format nil "make_symbol(\"~a\")" s)))))


;;; ==================================================
;;; Formatting
;;; ==================================================

(defun c-include (basename)
  (format nil "#include \"~a.h\"~%" basename))

(defun c-vardecl (basename value)
  (format nil "oop ~a_decls() { return ~a; }~%~%"
	  basename value))

(defun make-test-formatter (basename)
  (let ((test-basename (format nil "~a-test" basename))
	(var-basename (format nil "~a_test" basename)))
    #'(lambda (outstream cdecls)
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
		basename
		var-basename))))

(defun make-prod-formatter (basename)
  #'(lambda (outstream cdecls)
      (format outstream
	      "~a~%~a~%~a~a~%~a~%~a"
	      "// Auto-generated."
	      (c-include basename)
	      (c-include "cons")
	      (c-include "value")
	      (c-include "strings")
	      (c-vardecl basename
			 cdecls))))

(defun actual-convert (formatter file-basename)
  (format t " * Converting ~a.fn...~%" file-basename)
  (with-open-file (out
		   (format nil "~a.c" file-basename)
		   :direction :output
		   :if-exists :supersede)
    (with-open-file (in (format nil "~a.fn" file-basename))
      (apply formatter
	     (list out
		   (translate (read in)))))))


(defun convert-prod (basename)
  (actual-convert (make-prod-formatter basename)
		  basename))

(defun suffix? (str suffix)
  (and (<= (length suffix) (length str))
       (string= suffix
		(subseq str (- (length str)
			       (length suffix))))))

(defun strip-suffix (str suffix)
  (if (suffix? str suffix)
      (subseq str 0 (- (length str) (length suffix)))
      str))

(defun convert-test (basename)
  (actual-convert
   (make-test-formatter (strip-suffix basename "-test"))
   basename))

(defun run ()
  (dolist (basename (cdr sb-ext:*posix-argv*))
    (if (suffix? basename "-test")
	(convert-test basename)
	(convert-prod basename)))
  (quit :unix-status 0))


