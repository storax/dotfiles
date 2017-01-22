;;; packages.el --- storax-dabbrev layer packages file for Spacemacs.
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

(defconst storax-dabbrev-packages
  '(dabbrev))

(defun storax-dabbrev/init-dabbrev ()
    (use-package dabbrev
      :defer t
      :bind ("C-<tab>" . dabbrev-expand)
      :init
      (bind-key "C-<tab>" 'dabbrev-expand minibuffer-local-map)))

;;; packages.el ends here
