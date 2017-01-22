;;; config.el --- storax-helm layer config file for Spacemacs.
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

(defvar storax-helm-ag-dirs-prefix "sa SPC "
  "Prefix for all keybindings of `storax-helm-ag-dirs-alist'")
(defvar storax-helm-ag-dirs-alist '(("e" . "~/projects/emaci")
                                    ("o" . "~/Documents/org"))
  "Mapping of keys to directories for fast search access
with `storax/helm-ag-in-dir'.")

;;; config.el ends here

