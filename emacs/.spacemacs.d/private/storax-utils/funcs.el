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

(defcustom storax/buffers-to-keep nil
  "List of regexp for buffers to not kill."
  :type '(repeat string))

(defun storax/declare-buffer-bancrupcy ()
  "Kill a lot of buffers that you don't need."
  (interactive)
  (mapc
   (lambda (buffer)
     (let (save)
       (if (get-buffer-window buffer)
           (setq save t)
         (catch 'break
           (mapc
            (lambda (regexp)
              (when (string-match-p regexp (buffer-name buffer))
                (setq save t)
                (throw 'break nil)))
            storax/buffers-to-keep)))
          (unless save
            (kill-buffer buffer))))
   (buffer-list)))

;;; funcs.el ends here
