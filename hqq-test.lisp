;; this file is only temporary and isn't included in the actual asdf
;; system.  rather, it's a series of testing variables which can be
;; loaded for testing with other parts of the suite.  nevertheless, it
;; still gets a package for convenience.

(defpackage hqq/test
  (:use
   :cl
   :hqq/database)
  (:export
   :*test-todo*
   :*test-db*
   :*test-empty-db*)
  (:nicknames :hqqt))

(in-package :hqq/test)

(defvar *test-todo*
  (make-instance 'hqdb:hqq-todo
		 :priority 4
		 :doneness t
		 :date-of (make-instance 'hqdb:hqq-date-range
					 :begin-stamp
					 (get-universal-time))
		 :category 'another
		 :item-name "Gotta do it."
		 :note-of "I need to do this."))

;; stock test hqq-database, pretty much.
(defvar *test-db*
  (make-instance 'hqdb:hqq-database
		 :db-name "Testing TODOs"
		 :db-type 'hqdb:hqq-todo ;; most specific class so far.
		 :data-content
		 (make-array 1 :initial-element *test-todo*
			       :fill-pointer 1
			       :element-type 'hqdb:hqq-todo
			       :adjustable t)))

;; making sure categories won't get baleeted.  it could get tragic.
(defvar *test-empty-db*
  (make-instance 'hqdb:hqq-database
		 :db-name "Nothing important."
		 :categories '(this that another)
		 :db-type 'hqdb:hqq-item-note-date))
