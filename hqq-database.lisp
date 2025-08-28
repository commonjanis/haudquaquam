 (in-package :cl-user)

;; sorry about all the accessors.
(defpackage hqq/database
  (:use :cl)
  (:export
   :hqq-item
   :hqq-item-note-date
   :hqq-todo
   :hqq-database
   :new-modify-time
   :stamp-to-string-list
   :item-text-rep
   :hqq-date-range
   :read-an-item
   :nth-search-helper
   :nth-search
   :list-to-string
   :*item-text-rep-start*
   :*item-text-rep-end*
   :add-db-item
   :db-text-rep
   :data-content ; thus begin the accessors.
   :category
   :loaded-time
   :created-time
   :modified-time
   :item-name
   :item-total
   :item-id
   :imported-elsewhence
   :begin-stamp
   :end-stamp
   :note-of
   :date-of
   :doneness
   :priority
   :db-name
   :db-type
   :categories ; oops
   )
  (:nicknames :hqdb))

;; aw yeah.
(in-package :hqq/database)

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
;;
;; furthermore, people will get to roll their own specifications for
;; items if they so desire, so i may try to make macros or metaobject
;; specifications which deal with serializing as desired, but for now,
;; they can just code their own damn serialization on top of what
;; already exists.
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

;; there should also be an item validation group of methods at some
;; point, as well as a type detector, but i can wait to implement
;; that, i suppose.
(defgeneric read-an-item (rep kind)
  (:documentation "Read in some kind of item from rep - kind selects type."))

(defun nth-search-helper (substring string n start)
  (when (and start (not (string= string "")))
    (if (= n 1)
	(+ start (search substring string))
	(nth-search-helper substring
			   (subseq string (+ (length substring)
					     (search substring string)))
			   (1- n)
			   (+ start (search substring string) (length substring))))))

(defun nth-search (substring string n)
  (nth-search-helper substring string n 0))

;; TODO: optimize this bullshit with macros, perhaps just the simple
;; kind with an &body form taking all the basic definitions and
;; expanding thereupon.  i dunno; it's 20:48 and i'm too tired.
(defmethod read-an-item ((rep string) (kind (eql 'hqq-item-note-date)))
  (let* ((stripped-rep (string-left-trim " $iN" (string-right-trim " $o" rep)))
	 (id (parse-integer stripped-rep :junk-allowed t))
	 (category (read-from-string (subseq stripped-rep
					     (1+ (nth-search "|" stripped-rep 1))
					     (nth-search "|" stripped-rep 2))))
	 (name (subseq stripped-rep
		       (1+ (nth-search "|" stripped-rep 2))
		       (nth-search "|" stripped-rep 3)))
	 (created-modified
	   (subseq stripped-rep (+ 2 (search "|{" stripped-rep))
		   (search "}" stripped-rep)))
	 (created (parse-integer created-modified :junk-allowed t))
	 (modified (parse-integer (subseq created-modified
					  (1+ (search "|" created-modified)))
				  :junk-allowed t))
	 (relevant-dates
	   (make-instance 'hqq-date-range
			  :begin-stamp (parse-integer (subseq stripped-rep
							      (1+ (nth-search
								   "{"
								   stripped-rep
								   2))
							      (nth-search "|"
									  stripped-rep
									  5))
						      :junk-allowed t)
			  :end-stamp (parse-integer (subseq stripped-rep
							    (1+ (nth-search
								 "|"
								 stripped-rep
								 5)))
						    :junk-allowed t)))
	 (note (subseq stripped-rep (+ 3 (nth-search "+{+" stripped-rep 1))
		       (nth-search "+}+" stripped-rep 1))))
    (make-instance 'hqq-item-note-date
		   :imported-elsewhence t
		   :date-of relevant-dates
		   :note-of note
		   :category category
		   :created-time created
		   :modified-time modified
		   :item-id id
		   :item-name name)))

;; there's a lot of copypaste here.  it's probably best to find what
;; the two methods share in common and make a macro to establish the
;; common ground they share in the let* statement.
(defmethod read-an-item ((rep string) (kind (eql 'hqq-todo)))
  (let* ((stripped-rep (string-left-trim " $iNTD" (string-right-trim " $o" rep)))
	 (id (parse-integer stripped-rep :junk-allowed t))
	 (category (read-from-string (subseq stripped-rep
					     (1+ (nth-search "|" stripped-rep 1))
					     (nth-search "|" stripped-rep 2))))
	 (name (subseq stripped-rep
		       (1+ (nth-search "|" stripped-rep 2))
		       (nth-search "|" stripped-rep 3)))
	 (created-modified
	   (subseq stripped-rep (+ 2 (search "|{" stripped-rep))
		   (search "}" stripped-rep)))
	 (created (parse-integer created-modified :junk-allowed t))
	 (modified (parse-integer (subseq created-modified
					  (1+ (search "|" created-modified)))
				  :junk-allowed t))
	 (relevant-dates
	   (make-instance 'hqq-date-range
			  :begin-stamp (parse-integer (subseq stripped-rep
							      (1+ (nth-search
								   "{"
								   stripped-rep
								   2))
							      (nth-search "|"
									  stripped-rep
									  5))
						      :junk-allowed t)
			  :end-stamp (parse-integer (subseq stripped-rep
							    (1+ (nth-search
								 "|"
								 stripped-rep
								 5)))
						    :junk-allowed t)))
	 (note (subseq stripped-rep (+ 3 (nth-search "+{+" stripped-rep 1))
		       (nth-search "+}+" stripped-rep 1)))
	 (done (if (zerop (parse-integer (subseq stripped-rep
				      (+ 3 (nth-search "+}+" stripped-rep 1)))
					 :junk-allowed t))
		   nil t))
	 (prior (parse-integer (subseq stripped-rep
				       (1+ (nth-search "|" stripped-rep 6)))
			       :junk-allowed t)))
    (make-instance 'hqq-todo
		   :imported-elsewhence t
		   :date-of relevant-dates
		   :note-of note
		   :category category
		   :created-time created
		   :modified-time modified
		   :item-id id
		   :item-name name
		   :doneness done
		   :priority prior)))

;; TODO (albeit low on the range of priorities): implement a subclass
;; of hqq-item-note-date which deals with expenses, similarly to the
;; Expense database that comes with PalmOS.  it should allow for a
;; choice of payment method, amount of payment as a fixed-point number
;; (perhaps using an external library for the purpose), and a value
;; characterizing the reason for the payment, which should improve
;; upon the palm version by allowing users to choose from more than
;; just a pre-existing list.  it should be fully compatible with
;; existing methods.

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
   (categories :initarg :categories
	       :initform '()
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

;; this is what actually ensures there is some kind of valid
;; data-contents list.


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
      (setf content
	    (make-array 0
			:adjustable t
			:element-type type
			:fill-pointer 0)))
    (unless cats
      (if (> (length content) 0)
	  (setf cats
		(remove-duplicates (loop for thing across content
					 collecting (category thing))))))
    database))
  
;; TODO: helpful little function, method, or macro to construct an
;; array of hqq-items for use in a database more easily.  probably
;; also works with type specifiers somehow.

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
		    (not (eql each-cat t)) ; because t is a symbol!
		    (not (member cat prev-categories)))
	      do (push each-cat prev-categories)))
    prev-categories))

(defgeneric filter-db-category (database cat)
  (:documentation "Return a list of indices of items with cat, or t for everything."))

;; this macro takes first an item which is to be looped across, then a
;; placeholder var place-var, and finally a test to be used with the
;; if, which must act on place-var.  this was done so that the loops
;; wouldn't have to be written out the same damn way every time.
(defmacro filter-db-loop-helper (content-var place-var test)
  `(loop for ,place-var across ,content-var
	 for index from 0
	 if ,test ; must act on place-var.  maybe needs improvement.
	 collect index))

;; method specifically for when cat is NOT t or nil.
(defmethod filter-db-category ((database hqq-database) (cat symbol))
  (with-slots ((cats categories) (content data-content))
      database ;; below is a membership check.
    (when (and (member cat cats) (> (length content) 0) cats)
      (filter-db-loop-helper content item (eql (category item) cat)))))

;; specializing methods for handling cat being t and nil.  in
;; particular, this one returns every index in the database,
;; completely irrespective of its tag.  thus, it is optimized towards
;; this end and just returns every single index, irrespective of
;; tagging.  i'm sure there's a more efficient way to do the loop
;; macro portion, but who cares for now.
(defmethod filter-db-category ((database hqq-database) (cat (eql t)))
  (with-slots ((content data-content))
      database
    (when (> (length content) 0)
      (loop for index from 0 below (length content) collect index))))

;; the specializer for nil returns indices for any items which do
;; *not* have a category assigned to them.
(defmethod filter-db-category ((database hqq-database) (cat (eql nil)))
  (with-slots ((content data-content))
      database
    (when (> (length content) 0)
      (filter-db-loop-helper content item (not (category item))))))

;; in terms of implementation, this method should begin by ensuring
;; that everything in the argument "cat" is a non-nil, non-t symbol.
;; it then makes sure everything is actually in the tag list before it
;; runs the checking.  finally, we go through the list and return
;; indices for categories which are members of the cat list.
(defmethod filter-db-category ((database hqq-database) (cat list))
  (with-slots ((cats categories) (content data-content))
      database
    (let ((the-new-cats (remove-if (lambda (x)
				     (or (not (symbolp x))
					 (eql x t)
					 (not x)))
				   cat)))
      (when (and the-new-cats
		 (> (length content) 0)
		 (subsetp the-new-cats cats))
	(filter-db-loop-helper content item
			       (member (category item) the-new-cats))))))

;; TODO: string representation for databases and their contents in one
;; go.  this should have a similar overall structure but also
;; encapsulate the data associated with each item.  newlines should
;; separate everything as well, and furthermore, i think there should
;; be a reader function.  that's all!

(defgeneric add-db-item (database item)
  (:documentation "Add an item to a database - and a category if needed."))

;; type checking is done within the method rather than the specifier.
(defmethod add-db-item ((database hqq-database) item)
  (with-slots ((content data-content) (cats categories)
	       (the-type db-type)) ; note the specifier from the class.
      database
    (if (typep item the-type)
	(prog2
	    (vector-push-extend item content)
	    item
	  (unless (member (category item) cats)
	    (push (category item) cats)))
	nil)))

;; begins with the following: [rep start]DB|[data type]|[name]|+{+
;; [categories]+}+[newline]
;;
;; then, it continues from there down data-content, with each item
;; separated by a newline and printed in full.  finally, on a new
;; line, [rep end].
;;
;; also, note that two newlines together in the data-content section
;; means an empty hqq-database.
(defgeneric db-text-rep (database)
  (:documentation "Returns a flat string list representing an hqq-database."))

;; might change up the existing *item-text-rep-start* variables to be
;; more customizable in the context of the item-text-rep methods.
(defvar *db-text-rep-start* "!$")
(defvar *db-text-rep-end* "$!")

(defmethod db-text-rep ((database hqq-database))
  (with-slots ((content data-content)
	       (cats categories)
	       (name db-name)
	       (datatype db-type))
      database
    `(,*db-text-rep-start*
      "DB|"
      ,(write-to-string datatype)
      "|"
      ,name
      "|+{+"
      ,@(mapcar (lambda (x)
		  (concatenate 'string (write-to-string x) " "))
		cats) ; space-separated, including the last one.
      "+}+"
      #\Newline
      ,@(if (> (length content) 0)
	    (loop for item across content
		  collecting (list-to-string (append
					      (item-text-rep item)
					      '(#\Newline))))
	    '(#\Newline))
      ,*db-text-rep-end*)))

;; will rely on the database to consist of the type specified by the
;; text representation.
(defgeneric read-a-db (rep)
  (:documentation "Read a database from a string representation."))
