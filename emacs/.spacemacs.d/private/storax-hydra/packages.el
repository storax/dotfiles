;;; packages.el --- storax-hydra layer packages file for Spacemacs.
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

(defconst storax-hydra-packages
  '(move-text)
  "The list of Lisp packages required by the storax-hydra layer.")

(defun storax-hydra/post-init-move-text ()
  (defhydra hydra-move-text ()
    "Move text"
    ("p" move-text-up "up")
    ("n" move-text-down "down")
    ("q" nil "quit"))

  (spacemacs/set-leader-keys
    "xp" 'hydra-move-text/move-text-up
    "xn" 'hydra-move-text/move-text-down))

;;; packages.el ends here
