;; so far, this actually works!  i'll be damned.

(asdf:defsystem :hqq
    :description "haudquaquam: not quite a PalmOS-like database."
    :author "Janis Lago <janislago@gmail.com>"
    :licence "GPL 3.0"
    :depends-on (:clim :clim-lisp)
    :components ((:file "hqq-database")
		 (:file "hqq-gui" :depends-on ("hqq-database"))))
