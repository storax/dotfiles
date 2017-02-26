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

(defun storax-notmuch/mark-spam-show ()
  "Mark message as spam in show mode."
  (interactive)
  (notmuch-show-tag (list "+spam" "-inbox")))

(defun storax-notmuch/mark-spam-search ()
  "Mark message as spam in show mode."
  (interactive)
  (notmuch-search-tag (list "+spam" "-inbox"))
  (forward-line))

(defun storax-notmuch/mark-deleted-show ()
  "Mark message as deleted in show mode."
  (interactive)
  (notmuch-show-tag (list "+deleted" "-inbox")))

(defun storax-notmuch/mark-deleted-search ()
  "Mark message as deleted in show mode."
  (interactive)
  (notmuch-search-tag (list "+deleted" "-inbox"))
  (forward-line))


(defun storax-notmuch/init-notmuch ()
  "Init notmuch."
  (use-package notmuch
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
            notmuch-show-indent-messages-width 4))
    :config
    (progn
      (define-key notmuch-show-mode-map "S" 'storax-notmuch/mark-spam-show)
      (define-key notmuch-search-mode-map "S" 'storax-notmuch/mark-spam-search)
      (define-key notmuch-show-mode-map "d" 'storax-notmuch/mark-deleted-show)
      (define-key notmuch-search-mode-map "d" 'storax-notmuch/mark-deleted-search)
      (spacemacs/set-leader-keys "am" 'notmuch)
      (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime))))


;;; packages.el ends here
