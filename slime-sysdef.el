;;; slime-sysdef.el --- SYSDEF support for SLIME. -*- lexical-binding: t -*-

;; Author: Nicolas Martyanoff <nicolas@n16f.net>
;; SPDX-License-Identifier: ISC
;; URL: https://github.com/galdor/slime-sysdef
;; Version: 1.0.0
;; Package-Requires: (("emacs" "24.3") ("slime" "2.31"))

;;; Commentary:

;; We require Emacs 24.3 for cl-lib, and Slime 2.31 because Emacs does not
;; support package dependencies without a version number so we just took the
;; last one.

;;; Code:

(require 'cl-lib)
(require 'slime)

(define-slime-contrib slime-sysdef
  "SYSDEF support."
  (:authors "Nicolas Martyanoff <nicolas@n16f.net>")
  (:license "ISC")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-sysdef))

(defgroup slime-sysdef nil
  "SYSDEF support for Slime."
  :prefix "slime-sysdef-"
  :group 'slime)

(defun slime-sysdef-init ()
  (message "initializing slime-sysdef")
  ;; We use `file-truename' to handle the case where slime-sysdef.el is a
  ;; symlink to the original file in the cloned repository. This happens when
  ;; slime-sysdef is installed by the Straight package manager.
  (let* ((directory
          (file-name-directory (file-truename (locate-library "slime-sysdef"))))
         (swank-module-path (concat directory "swank-sysdef.lisp")))
    (slime-eval `(cl:load ,swank-module-path))))

(defslime-repl-shortcut slime-sysdef-repl--initialize-registry
                        ("initialize-registry")
                        (:handler 'slime-sysdef-initialize-registry)
                        (:one-liner "Initialize the SYSDEF system registry."))

(defun slime-sysdef-initialize-registry ()
  (interactive)
  (message "initializing SYSDEF system registry")
  (slime-repl-shortcut-eval-async `(sysdef:initialize-registry)
   (lambda (result)
     (message "SYSDEF system registry initialized"))))

(defslime-repl-shortcut slime-sysdef-repl--load-system ("load-system")
                        (:handler 'slime-sysdef-load-system)
                        (:one-liner "Build and load a SYSDEF system."))

(defun slime-sysdef-load-system (name)
  (interactive (list (slime-sysdef--read-system-name)))
  (message "loading SYSDEF system %S" name)
  (slime-repl-shortcut-eval-async
   `(sysdef:load-system (sysdef:system ,name))
   (lambda (result)
     (message "SYSDEF system %S loaded" name))))

(defun slime-sysdef--read-system-name ()
  (let* ((prompt "system: ")
         (names (slime-sysdef--list-system-names))
         (collection names)
         (predicate nil)
         (require-match nil)
         (initial-input nil)
         (history nil)
         (default-name nil)
         (completion-ignore-case t))
    (completing-read prompt collection predicate require-match initial-input
                     history default-name)))

(defun slime-sysdef--list-system-names ()
  (slime-eval '(cl:mapcar 'sysdef:system-name (sysdef:list-systems))))

(provide 'slime-sysdef)

;;; slime-sysdef.el ends here
