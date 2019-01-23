;;; packages.el --- storax-typescript layer packages file for Spacemacs.
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst storax-typescript-packages
  '(typescript-mode vue-mode))

(defun storax-typescript/init-vue-mode ()
  (use-package vue-mode))

(defun storax-typescript/post-init-typescript-mode ()
  (use-package typescript-mode
    :config
    (progn
      (add-hook 'vue-mode-hook 'flycheck-mode)
      (add-hook 'vue-mode-hook 'semantic-mode)
      (add-hook 'flycheck-mode-hook #'storax/use-tslint-from-node-modules)
      (advice-add #'typescript--ensure-cache :around #'storax/typescript--ensure-cache)
      (advice-add #'typescript--class-decl-matcher :around #'storax/typescript--class-decl-matcher))))

;;; packages.el ends here
