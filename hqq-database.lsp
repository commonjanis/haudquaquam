(defclass hqq-item ()
  ((category :initarg :category
	     :initform nil
	     :accessor category
	     :type symbol
	     :documentation "The category wherein an item exists.")
   (loaded-time :initform (get-universal-time)
		:reader loaded-time
		:type integer
		:documentation "Time an item was loaded at - not changeable.")
   (created-time :initform 0
		 :initarg :created-time
		 :accessor created-time
		 :type integer
		 :documentation "An item's creation time - immutable unless imported.")
   (modified-time :initform 0
		  :initarg :modified-time
		  :accessor modified-time
		  :type integer
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

(defclass hqq-date-range ()
  ((begin-stamp :initarg :begin-stamp
		:initform 0
		:accessor begin-stamp
		:type integer
		:documentation "The timestamp at which a date range begins.")
   (end-stamp :initarg :end-stamp
	      :initform 0
	      :accessor end-stamp
	      :type integer
	      :documentation "Point at which a date range ends.  Can equal begin-stamp.")))

(defmethod initialize-instance :after ((stamps hqq-date-range) &key)
  (progn
    (if (or (zerop (end-stamp stamps))
	    (> (end-stamp stamps) (begin-stamp stamps)))
	(setf (end-stamp stamps) (begin-stamp stamps)))
    (values (begin-stamp stamps) (end-stamp stamps))))

(defclass hqq-item-note-date (hqq-item)
  ((note :initarg :note
	 :initform ""
	 :accessor note-of
	 :type string
	 :documentation "An arbitrary note attached to an item.")
   (date-of :initarg :date-of
	    :accessor date-of
	    :type hqq-date-range
	    :documentation "A relevant hqq-date-range.")))

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

(defclass hqq-database ()
  ((data-content :initarg :data-content
		 :initform '()
		 :accessor data-content
		 :type list
		 :documentation "The contents of a given database.")
   (categories :initarg :categories
	       :initform '()
	       :accessor categories
	       :type list
	       :documentation "A list of symbols; a database's valid categories.")
   (db-name :initarg :db-name
	    :initform ""
	    :accessor db-name
	    :type string
	    :documentation "A database's name.")))
