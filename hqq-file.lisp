(in-package :cl-user)

(defpackage hqq/file
  (:use :cl :hqq/database)
  (:export
   :hqqs-already-in-file
   :save-hqq-to-file)
  (:nicknames :hqqf))

(in-package :hqq/file)

;; returns an alist with strings in the car positions, so make sure to
;; add :test #'string= or something like that when using "assoc" with
;; lists returned from here.
(defun hqqs-already-in-file (path)
  (let* ((begin-db (concatenate 'string
				hqdb:*db-text-rep-start* "DB"))
	 (begin-db-len (length begin-db)))
    (with-open-file (that path
			:if-does-not-exist :error)
      (loop for test-line = (read-line that nil 'conclude)
	    for line-index from 1
	    until (eq test-line 'conclude)
	    if (and (> (length test-line) 5) (string=
					      (subseq test-line 0 begin-db-len)
					      begin-db))
	      collect (cons (subseq test-line
				    (1+ (hqdb:nth-search "|" test-line 2))
				    (hqdb:nth-search "|" test-line 3))
			    line-index)))))

(defgeneric save-hqq-to-file (thing path)
  (:documentation "Save whatever hqq-related object to a file."))
