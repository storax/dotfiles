;;; packages.el --- storax-notmuch layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 David Zuber
;;
;; Author: David Zuber <zuber.david@gmx.de>
;; URL: https://github.com/storax/
;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:

;;; Code:
(defconst storax-notmuch-packages
  '(notmuch)
  "The list of Lisp packages required by the storax-notmuch layer.")

(defun storax-notmuch/post-init-notmuch ()
  "Init notmuch."
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (progn
      (require 'message)
      (require 'sendmail)
      (setq message-kill-buffer-on-exit t
            sendmail-program "msmtp"
            send-mail-function 'sendmail-send-it
            notmuch-fcc-dirs '(("zuber.david@gmx.de" . "gmx/Gesendet/"))
            notmuch-search-oldest-first nil
            notmuch-hello-thousands-separator ". "
            message-sendmail-envelope-from 'header
            mail-specify-envelope-from 'header
            notmuch-show-all-multipart/alternative-parts nil
            notmuch-crypto-process-mime t
            mml2015-encrypt-to-self t
            mml2015-sign-with-sender t
            mail-interactive t
            notmuch-show-indent-messages-width 4))))

;;; packages.el ends here
