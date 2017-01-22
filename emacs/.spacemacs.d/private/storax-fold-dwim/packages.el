;;; packages.el --- storax-fold-dwim layer packages file for Spacemacs.
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

(defconst storax-fold-dwim-packages
  '(fold-dwim))

(defun storax-fold-dwim/init-fold-dwim ()
  (use-package fold-dwim
    :commands (fold-dwim-hide-all fold-dwim-show-all fold-dwim-toggle-selective-display fold-dwim-toggle)
    :defer t
    :init
    (spacemacs/declare-prefix "nd" "fold" "fold-dwim")
    (spacemacs/set-leader-keys "ndh" 'fold-dwim-hide-all)
    (spacemacs/set-leader-keys "nds" 'fold-dwim-show-all)
    (spacemacs/set-leader-keys "ndT" 'fold-dwim-toggle)
    (spacemacs/set-leader-keys "ndt" 'fold-dwim-toggle-selective-display)))

;;; packages.el ends here
