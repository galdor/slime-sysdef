;;; slime-sysdef.el --- SYSDEF support for SLIME. -*- lexical-binding: t -*-

;; Author: Nicolas Martyanoff <nicolas@n16f.net>
;; SPDX-License-Identifier: ISC
;; URL: https://github.com/galdor/slime-sysdef
;; Version: 1.0.0
;; Package-Requires: ("emacs" "slime")

;;; Code:

(require 'cl-lib)
(require 'slime)

(define-slime-contrib slime-sysdef
  "SYSDEF support."
  (:authors "Nicolas Martyanoff <nicolas@n16f.net>")
  (:license "ISC")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-sysdef)
  (:on-load))

(defgroup slime-sysdef nil
  "SYSDEF support for Slime."
  :prefix "slime-sysdef-"
  :group 'slime)

(defun slime-sysdef-load-system (name)
  (interactive (list (slime-sysdef--read-system-name)))
  (message "loading system %S" name)
  (slime-repl-shortcut-eval-async
   `(swank:load-sysdef-system ,name)
   (lambda (result)
     (message "system %S loaded" name))))

(defslime-repl-shortcut slime-sysdef-repl--load-system ("load-system")
                        (:handler 'slime-sysdef-load-system)
                        (:one-liner "Build and load a SYSDEF system."))

(defun slime-sysdef--read-system-name ()
  (let* ((prompt "system: ")
         (names (slime-eval '(swank:list-sysdef-system-names)))
         (collection names)
         (predicate nil)
         (require-match nil)
         (initial-input nil)
         (history nil)
         (default-name nil)
         (completion-ignore-case t))
    (completing-read prompt collection predicate require-match initial-input
                     history default-name)))

(provide 'slime-sysdef)

;;; slime-sysdef.el ends here
