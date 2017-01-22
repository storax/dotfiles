;;; funcs.el --- storax-powerline layer functions file for Spacemacs.
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

;;----------------------------------------------------------------------------
;; Custom Functions for images
;;----------------------------------------------------------------------------
(defun storax/string-from-file (file)
  "Read FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun storax/color-svg (image color1)
  "Hacky stuff.  The svgs have %s for fill color.

IMAGE is the svg as string.
COLOR1 is the color to apply."
  (format image color1))

(defun storax-powerline/create-image (img color1 &optional acc)
  "Creates a image out of the a image data and colors it.
  ascent 90 seems to work best. mask is for transparent background"
  (create-image (storax/color-svg img color1) nil t :ascent (if acc acc 90) :mask 'heuristic))

(defun storax/create-image-with-face (image face)
  "Color IMAGE with the foreground color of FACE."
  (storax-powerline/create-image image (face-attribute face :foreground nil t)))

(defun storax/powerline-remote (face)
  "Return a github or bitbucket icon if remote points to one of them."
  (cond ((string-match "github.com" storax/remoteurl)
         (storax/create-image-with-face storax/github-mark-data face))
        ((string-match "bitbucket.org" storax/remoteurl)
         (storax/create-image-with-face storax/bitbucket-mark-data face))))

(defun storax/vc-git-working-revision (file)
  "Git-specific version of `vc-working-revision'."
  (let* (process-file-side-effects
         (str (vc-git--run-command-string nil "symbolic-ref" "HEAD"))
         parsed)
    (vc-file-setprop file 'vc-git-detached (null str))
    (setq parsed
          (if str
              (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
                  (match-string 2 str)
                str)
            (vc-git--rev-parse "HEAD")))
    (setq deactivate-mark nil)
    parsed))

(defun storax/powerline-vc ()
  (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
    (when backend
      (if (string= backend "Git")
          (format "%s %s" storax/icon-git
                  (storax/vc-git-working-revision (buffer-file-name (current-buffer))))
        (format "%s:%s" backend
                (vc-working-revision (buffer-file-name (current-buffer)) backend))))))

(defun storax/spaceline--theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-install

   `(,left
     anzu
     auto-compile
     ,second-left
     major-mode
     ((flycheck-error flycheck-warning flycheck-info)
      :when active)
     (((minor-modes :separator spaceline-minor-modes-separator)
       process)
      :when active)
     (erc-track :when active)
     (storax/vc-segment :when active)
     (org-pomodoro :when active)
     (org-clock :when active)
     nyan-cat)

   `(which-function
     (python-pyvenv :fallback python-pyenv)
     (storax/spotify-segment :when active)
     (battery :when active)
     selection-info
     input-method
     ((buffer-encoding-abbrev
       point-position
       line-column)
      :separator " | ")
     (global :when active)
     ,@additional-segments
     buffer-position
     hud)))

(defun storax/spaceline-spacemacs-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'storax/spaceline--theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :separator "|"
           :face highlight-face)
         '(buffer-modified buffer-size buffer-id remote-host)
         additional-segments))

;;; funcs.el ends here
