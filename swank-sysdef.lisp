(in-package :swank)

(defslimefun list-sysdef-system-names ()
  (mapcar 'sysdef:system-name (sysdef:list-systems)))

(defslimefun load-sysdef-system (name)
  (declare (type string name))
  (let ((system (sysdef:system name)))
    (sysdef:load-system system)))

(provide :swank-sysdef)
