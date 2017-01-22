;;; funcs.el --- storax-latex layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun latex-ae () "Insert ae."
       (interactive) (insert "\\\"a"))
(defun latex-oe () "Insert oe."
       (interactive) (insert "\\\"o"))
(defun latex-ue () "Insert ue."
       (interactive) (insert "\\\"u"))
(defun latex-ss () "Insert sharp s."
       (interactive) (insert "{\\ss}"))

(defun storax/apply-latex-bindings ()
  "Apply latex key bindings.

Why is this a function? And used in a hook?"
  (define-key latex-mode-map (kbd "C-' C-a") 'latex-ae)
  (define-key latex-mode-map (kbd "C-' C-o") 'latex-oe)
  (define-key latex-mode-map (kbd "C-' C-u") 'latex-ue)
  (define-key latex-mode-map (kbd "C-' C-s") 'latex-ss))

;;; funcs.el ends here
