;;; packages.el --- storax-yasnippet layer packages file for Spacemacs.
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

(defconst storax-yasnippet-packages
  '(yasnippet))

(defun storax-yasnippet/post-init-yasnippet ()
  (use-package yasnippet
    :init
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
    (spacemacs/add-to-hook 'text-mode-hook '(yas-minor-mode))
    :config
    (progn
      ;; Put spacemacs snippets to the front
      (let ((snippetdir "/home/david/.spacemacs.d/snippets"))
        (setq yas-snippet-dirs (remove snippetdir yas-snippet-dirs))
        (add-to-list 'yas-snippet-dirs snippetdir)))))

;;; packages.el ends here
