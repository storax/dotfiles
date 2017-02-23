;;; packages.el --- storax-i3 layer packages file for Spacemacs.
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
;; added to `storax-i3-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `storax-i3/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `storax-i3/pre-init-PACKAGE' and/or
;;   `storax-i3/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst storax-i3-packages
  '(i3wm
    (i3wm-config-mode
     :location (recipe :fetcher github :repo "Alexander-Miller/i3wm-Config-Mode")))
  "The list of Lisp packages required by the storax-i3 layer.")

(defun storax-i3/init-i3wm ()
  (use-package i3wm
    :defer t))

(defun storax-i3/init-i3wm-config-mode ()
    (use-package i3wm-config-mode
      :defer t
      :mode "\\.i3/config$"))
;;; packages.el ends here
