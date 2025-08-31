(in-package :cl-user)

(defpackage hqq/file
  (:use :cl :hqq/database)
  (:export
   :save-hqq-to-file)
  (:nicknames :hqqf))

(in-package :hqq/file)

(defgeneric save-hqq-to-file (thing path)
  (:documentation "Save whatever hqq-related object to a file."))
