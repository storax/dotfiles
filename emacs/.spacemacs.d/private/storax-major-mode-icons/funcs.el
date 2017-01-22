;;; funcs.el --- storax-helm-icons layer functions file for Spacemacs.
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
(defun storax/major-mode-icon (feature hook icon &optional prefix suffix)
  "After FEATURE is loaded add lambda to HOOK to replace the major mode text.

Text will be replaced with ICON and add an optional PREFIX and SUFFIX."
  (eval-after-load feature
    `(add-hook ',hook (lambda ()
                        (setq mode-name (concat ,prefix ,icon ,suffix))) t)))

(defun storax/add-major-mode-icon-hooks ()
  "Call 'storax/major-mode-icon' for each args in 'storax/major-mode-icon-list'."
  (dolist (e storax/major-mode-icon-list)
    (apply 'storax/major-mode-icon e)))

;;; funcs.el ends here
