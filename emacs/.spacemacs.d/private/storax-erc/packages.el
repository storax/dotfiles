;;; packages.el --- storax-erc layer packages file for Spacemacs.
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

(defconst storax-erc-packages
  '(erc persp-mode))

(defun storax-erc/post-init-erc ()
  (use-package erc
    :init
    (spacemacs/set-leader-keys "aig" 'storax-erc/erc-gitter)
    (spacemacs/set-leader-keys "aif" 'storax-erc/erc-freenode)
    :config
    (setq erc-server "irc.gitter.im"
          erc-modules
          '(autojoin
            button
            completion
            fill
            hl-nicks
            image
            irccontrols
            list
            log
            match
            menu
            move-to-prompt
            netsplit
            networks
            noncommands
            pcomplete
            readonly
            ring
            stamp
            track
            youtube)
          erc-autojoin-channels-alist
          '(("irc\.gitter\.im" "magit/magit" "syl20bnr/spacemacs" "signalpillar/chat-with-storax")
            (".*freenode.*" "#bitches")))))

(defun storax-erc/post-init-persp-mode ()
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'erc-mode)))
          persp-filter-save-buffers-functions))

  (spacemacs|define-custom-layout "@ERC"
    :binding "E"
    :body
   (progn
      (defun spacemacs-layouts/add-erc-buffer-to-persp ()
        (persp-add-buffer (current-buffer)
                          (persp-get-by-name
                           "@ERC")))
      (add-hook 'erc-mode-hook #'spacemacs-layouts/add-erc-buffer-to-persp)
      (if erc-server-list
          (erc/default-servers)
        (call-interactively 'storax-erc/erc-gitter))
      (erc :server "irc.freenode.net" :nick "notZubes"))))
;;; packages.el ends here
