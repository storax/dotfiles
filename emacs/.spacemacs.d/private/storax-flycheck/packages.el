;;; packages.el --- storax-flycheck layer packages file for Spacemacs.
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

(defconst storax-flycheck-packages
  '(flycheck)
  "The list of Lisp packages required by the storax-flycheck layer.")

(defun storax-flycheck/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config
    (defun storax/indicate-error-nav-wrapped (direction)
      "Display a message in minibuffer also flash the mode-line.

DIRECTION inidactes if 'next or 'previous was used."
      (let ((mode-line-color (face-background 'mode-line)))
        (set-face-background 'mode-line storax/modeline-flash-color)
        (sit-for 0.3)
        (set-face-background 'mode-line mode-line-color)))

    (defun storax/next-error-wrapped (&optional arg reset)
      "Jumps to previous error if at first error jump to last error instead.

Prefix argument ARG says how many error messages to move forwards (or
backwards, if negative).  With just prefix moves to first error.
RESET does nothing."
      (interactive "P")
      (condition-case nil
          (if (flycheck-has-current-errors-p)
              (call-interactively 'flycheck-next-error))
        (error (progn (flycheck-next-error 1 t)
                      (storax/indicate-error-nav-wrapped 'next)))))

    (defun storax/jump-to-last-error (buffer)
      "Jump to last error in the BUFFER, this assumes that the error is at last but third line."
      (save-selected-window
        (select-window (get-buffer-window buffer))
        (goto-char (point-max))
        (forward-line -3)
        (call-interactively 'compile-goto-error)))

    (defun storax/previous-error-wrapped (&optional arg)
      "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move backwards (or
forwards, if negative)."
      (interactive "P")
      (if (flycheck-has-current-errors-p)
          (condition-case nil
              (if (eq (point) (point-min))
                  (progn
                    (storax/indicate-error-nav-wrapped 'previous)
                    (goto-char (point-max))
                    (call-interactively 'flycheck-previous-error))
                (if (compilation-buffer-p (current-buffer))
                    (compilation-previous-error 1)
                  (call-interactively 'flycheck-previous-error)))
            (error (progn
                     (let ((error-buffer (next-error-find-buffer)))
                       ;; If the buffer has an associated error buffer use it to
                       ;; to move to last error
                       (if (and (not (eq (current-buffer) error-buffer))
                                (compilation-buffer-p error-buffer))
                           (storax/jump-to-last-error error-buffer)
                         ;; Otherwise move to last point and invoke previous error
                         (goto-char (point-max))
                         (call-interactively 'flycheck-previous-error)
                         (storax/indicate-error-nav-wrapped 'previous))))))))

    (setq flycheck-emacs-lisp-load-path load-path
          flycheck-idle-change-delay 0.8
          flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

    (global-flycheck-mode t)
    (bind-key "C-c C-n" 'storax/next-error-wrapped)
    (bind-key "C-c C-p" 'storax/previous-error-wrapped)
    (spacemacs/set-leader-keys "en" 'storax/next-error-wrapped)
    (spacemacs/set-leader-keys "ep" 'storax/previous-error-wrapped)))

;;; packages.el ends here

