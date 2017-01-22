;;; packages.el --- storax-unkillable-scratch layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: David Zuber <david@dztesla>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst storax-unkillable-scratch-packages
  '(unkillable-scratch)
  "The list of Lisp packages required by the storax-unkillable-scratch layer.")

(defun storax-unkillable-scratch/init-unkillable-scratch ()
    "Initialize unkillable-scratch"
  (unkillable-scratch 1))

;;; packages.el ends here
