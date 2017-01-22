;;; packages.el --- storax-dash layer packages file for Spacemacs.
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

(defconst storax-dash-packages
  '(helm-dash))

(defun storax-dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    (setq helm-dash-browser-func 'eww)
    (spacemacs/declare-prefix "d" "dash")
    (spacemacs/set-leader-keys
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash
      "di" 'helm-dash-install-docset
      "du" 'helm-dash-install-user-docset)
    :config
    (defun storax/dash-install-set (docset)
      "Install the DOCSET if it does not exist already."
      (let* ((escaped (when (listp docset) (cdr docset)))
             (docset (if (listp docset) (car docset) docset))
             (name-escaped (or escaped (replace-regexp-in-string "_" " " docset))))
        (unless (file-exists-p (concat storax/dash-docsets-path (concat name-escaped ".docset")))
          (helm-dash-install-docset docset))))

    (defun storax/dash-install-user-set (docset)
      "Install the user DOCSET if it does not exist already."
      (let* ((escaped (when (listp docset) (cdr docset)))
             (docset (if (listp docset) (car docset) docset))
             (name-escaped (or escaped (replace-regexp-in-string "_" " " docset))))
        (unless (file-exists-p (concat storax/dash-docsets-path (concat name-escaped ".docset")))
          (helm-dash-install-user-docset docset))))
    (dolist (docset storax/docsets-to-install)
      (storax/dash-install-set docset))
    (dolist (docset storax/user-docsets-to-install)
      (storax/dash-install-user-set docset))
    (setq helm-dash-common-docsets storax/dash-common-docsets)))

;;; packages.el ends here
