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
  '(persp-mode
    ;; magithub
    )
  "The list of Lisp packages required by the storax-magit layer.")

(defun storax-magit/post-init-persp-mode ()
  (use-package persp-mode
    :init
    (progn
      (spacemacs/transient-state-register-add-bindings "layouts"
        '(("m" storax-magit/persp-for-repo :exit t))))))

;; (defun storax-magit/init-magithub ()
;;   (use-package magithub
;;     :after magit
;;     :config (magithub-feature-autoinject t)))


(defun storax-magit/persp-for-repo (name path)
  (interactive (let ((repos (magit-repos-alist)))
                 (let ((reply (assoc (magit-completing-read "Git repositories" (magit-repos-alist)) repos)))
                   (list (car reply) (cdr reply)))))
  (persp-switch name)
  (magit-status path))



;;; packages.el ends here
