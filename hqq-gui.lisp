;; pretty much just a placeholder for now.

(in-package :common-lisp-user)

(defpackage hqq/gui
  (:use :hqq/database
	:clim
	:clim-lisp)
  (:nicknames :hqqg))

(in-package :hqq/gui)

;; TODO (very big picture): implement a means to interact with various
;; hqq databases within a straightforward, conveniently single-window
;; application.  i will start simple and build from there.  also, this
;; file should consistently use the "hqdb:" alias for "hqq/database:"
;; because that makes things much easier to write out and probably
;; also understand.
