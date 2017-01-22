;;; packages.el --- storax-smartparens layer packages file for Spacemacs.
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

(defconst storax-smartparens-packages
  '(smartparens)
  "The list of Lisp packages required by the storax-smartparens layer.")

(defun storax-smartparens/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (smartparens-global-mode 1)
    (sp-use-paredit-bindings)
    :config
    (bind-key "C-M-." 'storax/swap-place-in-region)
    (bind-key "C-M-," 'storax/swap-place-in-region)))

;;; packages.el ends here
