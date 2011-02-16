
;; (convert "utils.fn" "utils.h")

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

(defun convert (in-file out-file)
  (with-open-file (out
		   out-file
		   :direction :output
		   :if-exists :supersede)
    (with-open-file (in in-file)
      (format out (translate (read in))))))