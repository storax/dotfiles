;;; packages.el --- storax-projectile layer packages file for Spacemacs.
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

(defconst storax-projectile-packages
  '(projectile)
  "The list of Lisp packages required by the storax-projectile layer.")

(defun storax-projectile/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (projectile-register-project-type 'python-tox2 '("tox.ini") nil "tox")

    (defun storax/projectile-test-prefix (project-type)
      "Find default test files prefix based on PROJECT-TYPE."
      (cond
       ((member project-type '(django python-pip python-pkg python-tox python-tox2)) "test_")
       ((member project-type '(emacs-cask)) "test-")
       ((member project-type '(lein-midje)) "t_")))

    (setq projectile-test-prefix-function 'storax/projectile-test-prefix
          projectile-completion-system 'helm)))


;;; packages.el ends here
