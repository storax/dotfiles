;;; funcs.el --- storax-utils layer functions file for Spacemacs.
;;
;; Copyright (c) 2016 David Zuber
;;
;; Author David Zuber <david.zuber@gmx.de>
;; URL: https://github.com/storax/emacs-castle
;;
;; This file is not part of GNU Emacs
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------------
;; Handier way to access items
;;----------------------------------------------------------------------------
(unless (fboundp 'alist-get) ;;25.1
  (defun alist-get (key alist &optional default remove)
    "Get the value associated to KEY in ALIST.

DEFAULT is the value to return if KEY is not found in ALIST.
REMOVE, if non-nil, means that when setting this element, we should
remove the entry if the new value is `eql' to DEFAULT."
    (ignore remove) ;;Silence byte-compiler.
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;; funcs.el ends here
