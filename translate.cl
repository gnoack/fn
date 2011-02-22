
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
  (format nil "make_string(\"~a\")" str))

(defun translate-char (ch)
  (format nil "make_char('~a')" ch))

(defun translate-symbol (sym)
  (let ((s (string-downcase (string sym))))
    (cond ((string= "nil" s) "NIL")
	  (t (format nil "make_symbol(\"~a\")" s)))))

(defun c-include (basename)
  (format nil "#include \"~a.h\"~%" basename))

(defun c-vardecl (basename value)
  (format nil "extern~%oop ~a_decls = ~a;~%"
	  basename value))

;; Converts the fn-file with the given basename into a
;; c-file.
(defun convert (basename)
  (with-open-file (out
		   (format nil "~a.c" basename)
		   :direction :output
		   :if-exists :supersede)
    (with-open-file (in (format nil "~a.fn" basename))
      (format out
	      "~a~%~a~a~%~a"
	      (c-include basename)
	      (c-include "cons")
	      (c-include "value")
	      (c-vardecl basename
			 (translate (read in)))))))

;; (convert "utils")

