;;; packages.el --- storax-popup layer packages file for Spacemacs.
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

(defconst storax-popup-packages
  '(popup))

(defun storax-popup/post-init-popup ()
  (use-package popup
    :defer t
    :config
    (bind-key "M-n" 'popup-next popup-menu-keymap)
    (bind-key "TAB" 'popup-next popup-menu-keymap)
    (bind-key "<tab>" 'popup-next popup-menu-keymap)
    (bind-key "<backtab>" 'popup-previous popup-menu-keymap)
    (bind-key "M-p" 'popup-previous popup-menu-keymap)))

;;; packages.el ends here
