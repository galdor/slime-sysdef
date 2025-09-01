(defpackage swank-sysdef
  (:use :cl)
  (:export
   #:list-system-names))

(in-package :swank-sysdef)

(defun list-system-names ()
  (mapcar 'sysdef:system-name (sysdef:list-systems)))

;; Probably useless since we do not rely on slime-require in slime-sysdef.el.
(provide :swank-sysdef)
