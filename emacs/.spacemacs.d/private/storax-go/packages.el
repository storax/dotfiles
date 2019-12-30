;;; packages.el --- storax-go layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
;; added to `storax-go-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `storax-go/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `storax-go/pre-init-PACKAGE' and/or
;;   `storax-go/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst storax-go-packages
  '(dap-mode)
  "The list of Lisp packages required by the storax-go layer.")

(defun storax-go/pre-init-dap-mode ()
  (add-to-list 'spacemacs--dap-supported-modes 'go-mode)
  (add-hook 'go-mode-local-vars-hook #'storax-go/go-setup-lsp-dap))


(defun storax-go/post-init-dap-mode ()
  (use-package dap-mode
    :config
    (progn
      (dap-register-debug-provider "go" 'storax-go/dap-go-populate-default-args))))

;;; packages.el ends here
