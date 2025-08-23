(defclass hqq-item ()
  ((category :initarg :category
	     :initform nil
	     :accessor category
	     :type symbol
	     :documentation "The category wherein an item exists.")
   (loaded-time :initform (get-universal-time)
		:reader loaded-time
		:type (integer 0 *)
		:documentation "Time an item was loaded at - not changeable.")
   (created-time :initform 0
		 :initarg :created-time
		 :accessor created-time
		 :type (integer 0 *)
		 :documentation "An item's creation time - immutable unless imported.")
   (modified-time :initform 0
		  :initarg :modified-time
		  :accessor modified-time
		  :type (integer 0 *)
		  :documentation "An item's modification time - changeable!")
   (item-name :initarg :item-name
	      :initform ""
	      :accessor item-name
	      :type string
	      :documentation "Arbitrary name of the database item.")
   (item-total :allocation :class
	       :initform 0
	       :accessor item-total
	       :type integer
	       :documentation "Overall number of items used.  Don't mess it up.")
   (item-id :accessor item-id
	    :initarg :item-id
	    :initform 0
	    :type integer
	    :documentation "ID of an item, to be derived from item-total.")
   (imported-elsewhence :initarg :imported-elsewhence
			:initform nil
			:type boolean
			:accessor imported-elsewhence
			:documentation "Whether this item came from an outside source.")))

(defmethod initialize-instance :after ((item hqq-item) &key)  
  (prog1
      (if (imported-elsewhence item)
	  (with-slots ((id item-id) (total item-total))
	      item
	    (cond ((> id total)
		   (setf total id))
		  ((= id total)
		   (setf total (1+ id)))
		  (t id)))
	  (setf (item-id item) (incf (item-total item))))
    (with-slots ((created created-time)
		 (modified modified-time)
		 (loaded loaded-time))
	item
      (if (or (zerop created) (zerop modified))
	  (setf created loaded
		modified loaded)))))

;; use this whenever anything of significance changes about an hqq-item
(defgeneric new-modify-time (item)
  (:documentation "Generically change the modified-time of any hqq-item."))

(defmethod new-modify-time ((item hqq-item))
  (setf (modified-time item) (get-universal-time)))

;; TODO: serialization for the various hqq-item-derived types, but
;; probably for hqq-item itself first and foremost.  i want to use a
;; simple but compact method to implement this kind of thing.  below
;; is the most likely candidate for a generic to be used here.
(defvar *item-text-rep-start* "$$i")
(defvar *item-text-rep-end* "$$o")

(defgeneric item-text-rep (item)
  (:documentation "Group of methods for making strings from hqq-item objects."))

;; NOT intended to be used on its own in practice.  this is just a
;; basic template for other methods on derived classes to make strings
;; out of this output.
;;
;; note likewise the format, which is of the format [type tag
;; position] [id] PIPE [category] PIPE [item name] PIPE [timestamps
;; enclosed in curly brackets and separated by another pipe].  this is
;; what all the others build on, so for efficiency's sake, i haven't
;; added beginning and ending tags to this particular method.
(defmethod item-text-rep ((item hqq-item))
  (with-slots ((cat category) (name item-name)
	       (create created-time) (mod modified-time)
	       (id item-id))
      item
    (list (write-to-string id)
	  "|"
	  (string cat)
	  "|"
	  name ; can't contain the pipe character, so filter it later.
	  "|"
	  "{"
	  (write-to-string create) ; order is always creation then
				   ; modification, or more broadly
				   ; beginning then end.
	  "|"
	  (write-to-string mod)
	  "}")))

(defclass hqq-date-range ()
  ((begin-stamp :initarg :begin-stamp
		:initform 0
		:accessor begin-stamp
		:type (integer 0 *)
		:documentation "The timestamp at which a date range begins.")
   (end-stamp :initarg :end-stamp
	      :initform 0
	      :accessor end-stamp
	      :type (integer 0 *)
	      :documentation "Point at which a date range ends.  Can equal begin-stamp.")))

(defmethod initialize-instance :after ((stamps hqq-date-range) &key)
  (with-slots ((end end-stamp) (begin begin-stamp))
      stamps
    (progn
      (if (or (zerop end)
	      (< end begin))
	  (setf end begin))
      (values begin end))))

(defgeneric stamp-to-string-list (stamp)
  (:documentation "Just turns an hqq-date-range into a list of strings."))

;; found via MatthewRock in a github gist comment section.  LOL.
(defun list-to-string (lst) (format nil "~{~A~}" lst))

;; for convenience in full string representations.
(defmethod stamp-to-string-list ((stamp hqq-date-range))
  (list "{"
	(write-to-string (begin-stamp stamp))
	"|"
	(write-to-string (end-stamp stamp))
	"}"))

(defclass hqq-item-note-date (hqq-item)
  ((note-of :initarg :note-of
	    :initform ""
	    :accessor note-of
	    :type string
	    :documentation "An arbitrary note attached to an item.")
   (date-of :initarg :date-of
	    :accessor date-of
	    :type hqq-date-range
	    :documentation "A relevant hqq-date-range.")))

;; returns a list of strings, to be concatenated later on - see the
;; above function list-to-string.  there should also be a unique
;; series of identifiers after item-text-rep-start (whatever its value
;; may be), and those (each one non-numeric character) will determine
;; how the rest of the data is parsed later on.
;;
;; also, the format is the same as for the hqq-item base, but with the
;; addition of a representation of the date timestamps - identical to
;; the creation-modification rep but succeeding it - followed by a
;; note delimited +{+like this+}+ at the end.
(defmethod item-text-rep :around ((item hqq-item-note-date))
  (with-slots ((date date-of) (note note-of))
      item
    `(,*item-text-rep-start* "N" ,@(call-next-method)
			     ,@(stamp-to-string-list date)
			     "+{+" ; begins a note.
			     ,note
			     "+}+" ; ends a note.
			     ,*item-text-rep-end*)))

(defclass hqq-todo (hqq-item-note-date)
  ((doneness :initarg :doneness
	     :initform nil
	     :accessor doneness
	     :type boolean
	     :documentation "Whether the item is actually done.")
   (priority :initarg :priority
	     :initform 1 ;; considered the highest priority
	     :accessor priority
	     :type (integer 1 5)
	     :documentation "How urgent an item is, with 1 being top priority.")))

;; specification is similar to hqq-item-note-date, but there's a pair
;; of numbers separated by a pipe near the end, which are,
;; respectively, the doneness state (0 or 1) and the priority value.
;; otherwise, it's pretty much identical.
(defmethod item-text-rep :around ((item hqq-todo))
  (with-slots ((done doneness) (prior priority))
      item
    (let ((main-material (cddr (call-next-method))))
      `(,*item-text-rep-start* ; the opening item
	"NTD" ; signal for "note todo"
	,@(reverse (cdr (reverse main-material))) ; all else but the last item
	,(if done "1" "0")
	"|"
	,(write-to-string prior)
	,*item-text-rep-end*))))

;; the type specifier solution with db-type is pretty awesome, but
;; there should be some means by which one could check whether the
;; type specifier provided is actually among hqq-item or its various
;; defined subclasses (at runtime), so i'm looking into the metaobject
;; protocols of common lisp towards that end.
(defclass hqq-database ()
  ((data-content :initarg :data-content
		 :accessor data-content
		 :type array
		 :documentation "The contents of a given database.")
   (categories :initform '()
	       :accessor categories
	       :type list
	       :documentation "A list of symbols; a database's valid categories.")
   (db-name :initarg :db-name
	    :initform ""
	    :accessor db-name
	    :type string
	    :documentation "A database's name.")
   (db-type :initarg :db-type
	    :initform 'hqq-item
	    :accessor db-type
	    :type symbol
	    :documentation "The specific type of hqq-item this database uses.")))

;; begins by ensuring the database's contents have the right kind of
;; array, then, if there are no categories defined and the database
;; has at least one item, tries to make a list of categories from the
;; contents of the database.  i still need to test this, but i'll go
;; ahead and commit it for posterity.
(defmethod initialize-instance :after ((database hqq-database) &key)
  (with-slots ((cats categories) (content data-content)
	       (type db-type))
      database
    (unless (slot-boundp database 'data-content)
      (setf content (make-array 0 :adjustable t :element-type type)))
    (unless (and cats (> (length content) 0))
      (setf cats
	    (remove-duplicates (loop for thing across content
				     collecting (category thing)))))))

(defgeneric add-category (database cat)
  (:documentation "Add a new category to a database if not already there."))

(defmethod add-category ((database hqq-database) (cat symbol))
  (with-slots ((prev-categories categories))
      database
    (if (not (member cat prev-categories))
	(push cat prev-categories)
	prev-categories)))

;; similar version, but with a list of multiple categories.
(defmethod add-category ((database hqq-database) (cat list))
  (with-slots ((prev-categories categories))
      database
    (when cat
      (loop for each-cat in cat
	    if (and (symbolp each-cat) each-cat
		    (not (member cat prev-categories)))
	      do (push each-cat prev-categories)))
    prev-categories))
