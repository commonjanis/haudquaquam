(defpackage :hqq-database
  (:export hqq-item hqq-item-note-date new-modify-time))

(defclass hqq-item ()
  ((category :initarg :category
	     :initform nil
	     :accessor category
	     :type symbol
	     :documentation "The category wherein an item exists.")
   (created-time :initform (get-universal-time)
		 :reader created-time
		 :type integer
		 :documentation "An item's creation time - not changeable.")
   (modified-time :initform (get-universal-time)
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
	    :type integer
	    :documentation "ID of an item, to be derived from item-total.")))

(defmethod initialize-instance :after ((item hqq-item) &key)
  (setf (item-id item) (incf (item-total item))))

(defgeneric new-modify-time (item)
  (:documentation "Generically change the modified-time of any hqq-item."))

(defmethod new-modify-time ((item hqq-item))
  (setf (modified-time item) (get-universal-time)))

(defclass hqq-item-note-date (hqq-item)
  ((note :initarg :note
	 :initform ""
	 :accessor note
	 :type string
	 :documentation "An arbitrary note attached to an item.")))
