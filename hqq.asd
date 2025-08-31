;; so far, this actually works!  i'll be damned.
;;
;; now to actually try out the :serial option.

(asdf:defsystem :hqq
    :description "haudquaquam: not quite a PalmOS-like database."
    :author "Janis Lago <janislago@gmail.com>"
    :licence "GPL 3.0"
    :depends-on (:clim :clim-lisp)
    :serial t
    :components ((:file "hqq-database")
		 (:file "hqq-file")
		 (:file "hqq-gui")))
