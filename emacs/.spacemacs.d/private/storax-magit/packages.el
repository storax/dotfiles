;;; packages.el --- storax-magit layer packages file for Spacemacs.
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

(defconst storax-magit-packages
  '(magithub)
  "The list of Lisp packages required by the storax-magit layer.")

(defun storax-magit/init-magithub ()
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t)))
;;; packages.el ends here
