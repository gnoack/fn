
(defun concatenate-string (&rest strings)
  (apply #'concatenate 'string strings))

(defun join-strings (&rest list)
  #!+sb-doc
  "Joins a list of strings using a space sign."
  (subseq (apply #'concatenate-string
		 (mapcar #'(lambda (str)
			     (concatenate 'string " " str))
			 list))
	  1))

(defun translate (input)
  #!+sb-doc
  "Translates an input program into a C-readable form."
  (cond ((listp input) (translate-list input))
	(t (string input))))

(defun translate-list (list)
  (concatenate 'string
	       "LIST("
	       (apply #'join-strings
		      (mapcar #'translate list))
	       ")"))

