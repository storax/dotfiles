;;; packages.el --- storax-powerline layer packages file for Spacemacs.
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

(defconst storax-powerline-packages
  '(spaceline magit))

(defun storax-powerline/post-init-spaceline ()
  (use-package spaceline-config
    :config
    (spaceline-define-segment storax/vc-segment
      (list (storax/powerline-remote default-face) (storax/powerline-vc))
      :when (or vc-mode (string-match "magit" (format "%s" major-mode))))
    (spaceline-define-segment storax/spotify-segment
      (list (storax/create-image-with-face storax/spotify-data default-face))
      :when storax/spotify-connected)
    (storax/spaceline-spacemacs-theme '(new-version :when active))))

(defun storax-powerline/post-init-magit ()
  (use-package magit
    :config
    (defun storax/setremoteurl ()
      "Save the current remote url."
      (make-local-variable 'storax/remoteurl)
      (setq storax/remoteurl (magit-get "remote" "origin" "url")))
    ;; Kinda like each time we open a file we set the storax/remoteurl
    ;; then we can display an icon in the modeline accordingly
    (add-hook 'after-change-major-mode-hook 'storax/setremoteurl)))

;;; packages.el ends here
