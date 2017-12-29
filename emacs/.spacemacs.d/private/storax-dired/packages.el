;;; packages.el --- storax-dired layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `storax-dired-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `storax-dired/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `storax-dired/pre-init-PACKAGE' and/or
;;   `storax-dired/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst storax-dired-packages
  '(dired dired-narrow)
  "The list of Lisp packages required by the storax-dired layer.")

(defun storax-dired/post-init-dired ()
  (use-package dired
    :init
    (setq dired-listing-switches "-alh")))

(defun storax-dired/init-dired-narrow ()
  (use-package dired-narrow
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

;;; packages.el ends here
