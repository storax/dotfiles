;;; packages.el --- storax-semantic layer packages file for Spacemacs.
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
;; added to `storax-semantic-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `storax-semantic/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `storax-semantic/pre-init-PACKAGE' and/or
;;   `storax-semantic/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst storax-semantic-packages
  '(helm))

(setq storax-semantic-excluded-packages '(stickyfunc-enhance))

(defun semantic/enable-semantic-mode (mode)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda ()
                     (require 'semantic)
                     (add-to-list 'semantic-default-submodes
                                  'global-semantic-idle-summary-mode)
                     (semantic-mode 1)))))

(defun storax-semantic/post-init-helm ()
  (use-package helm
    :defer t
    :config
    (spacemacs/set-leader-keys "so" 'helm-semantic-or-imenu)))

;;; packages.el ends here
