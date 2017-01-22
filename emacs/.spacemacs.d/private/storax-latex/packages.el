;;; packages.el --- storax-latex layer packages file for Spacemacs.
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

(defconst storax-latex-packages
  '(auctex pdf-tools))

(defun storax-latex/post-init-auctex ()
  (use-package auctex
    :defer t
    :config
    (TeX-global-PDF-mode t)
    (setq TeX-source-correlate-start-server t)
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    ;; use special hook for tex-default-command, as it is a local variable
    (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'storax/apply-latex-bindings)))

(defun storax-latex/init-pdf-tools ()
  (use-package pdf-tools
    :config
    (pdf-tools-install)))

;;; packages.el ends here
