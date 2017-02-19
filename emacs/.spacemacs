;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     php
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ansible
     ;;asciidoc
     auto-completion
     better-defaults
     ;;django
     deft
     emacs-lisp
     (erc :packages (not persp-mode))
     git
     html
     javascript
     latex
     markdown
     org
     (python :variables
             python-test-runner nil)
     ruby
     search-engine
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     ;;spacemacs-layouts
     syntax-checking
     (theming
      :variables theming-modifications
      '((zenburn (avy-background-face :background "#3F3F3F" :foreground "#757565" :inverse-video nil)
                 (avy-lead-face :background "#E8BF6A" :foreground "black" :inverse-video nil)
                 (avy-lead-face-0 :background "#C45837" :foreground "white" :inverse-video nil)
                 (avy-lead-face-1 :background "#6D9CBE" :foreground "black")
                 (avy-lead-face-2 :background "#B4C973" :foreground "black")
                 (diff-hl-change :background "#4F4F4F" :foreground "#FF88FF")
                 (diff-hl-delete :background "#4F4F4F" :foreground "#FF5353")
                 (diff-hl-insert :background "#4F4F4F" :foreground "#7FFF7F")
                 (font-latex-sectioning-5-face :inherit fixed-pitch :foreground "#CC9393" :weight bold)
                 (git-gutter:modified :background "#DC8CC3" :foreground "#3F3F3F" :weight bold)
                 (mode-line :background "#2B2B2B" :foreground "#8FB28F" :box nil :overline "dim gray")
                 (mode-line-inactive :background "#383838" :foreground "#5F7F5F" :box nil :overline "dim gray")
                 (powerline-inactive1 :inherit mode-line-inactive :background "#4F4F4F" :foreground "#7F9F7F")
                 (powerline-inactive2 :inherit mode-line-inactive :background "#6F6F6F" :foreground "#9FC29F")
                 (region :background "#222222")
                 (scroll-bar :background "#545450" :foreground "#3F3F3F"))))
     ;;vagrant
     ;; version-control
     xkcd
     yaml
     storax-icons
     storax-utils
     storax-unkillable-scratch
     storax-avy
     storax-popup
     storax-dabbrev
     storax-fold-dwim
     storax-helm
     storax-helm-icons
     storax-hydra
     storax-smartparens
     storax-flycheck
     storax-projectile
     storax-python
     storax-org
     ;;storax-latex
     (storax-dash
      :variables
      storax/docsets-to-install
      '("Ansible" "Bash" "Boost" "C" "C++" "CMake" "Chef" "Common_Lisp" "Clojure"
        "Django" "Docker" "ElasticSearch" "Emacs_Lisp" "Flask" "Jinja"
        "LaTeX" "PostgreSQL" "Python_2" "Python_3" "SQLAlchemy"
        "Vagrant" ("Qt_4" . "Qt") ("Emacs" . "emacs"))
      storax/user-docsets-to-install
      '("Alembic" "Packer" "PyMel" "Requests" "Sphinx")
      storax/dash-common-docsets
      '("Bash" "Common Lisp" "Clojure" "Emacs Lisp" "Packer" "Python 2" "Python 3"
        "Qt" "Requests" "Sphinx" "Vagrant" "emacs"))
     storax-visual-regexp-steroids
     ;; storax-major-mode-icons
     ;;storax-mode-icons
     storax-translate
     ;; storax-powerline
     storax-semantic
     storax-erc
     storax-yasnippet
     storax-notmuch
     storax-dired
     storax-magit
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(org-tfl rainbow-mode epc)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(vi-tilde-fringe helm-gitignore org-alert refine)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key "SPC"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (setq user-full-name "David Zuber"
        user-mail-address "zuber.david@gmx.de"
        vs-follow-symlinks t ; When following sysmlinks always go to the destination
        require-final-newline t
        indicate-empty-lines t)
  (add-to-list 'load-path (concat dotspacemacs-directory "elisp")))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (ido-mode -1)
  (global-subword-mode 1)
  ;; nicer regexp syntax
  (use-package re-builder
    :defer t
    :config
    (setq reb-re-syntax 'string))
  ;; layouts
  (spacemacs|define-custom-layout "@Spacemacs"
    :binding "e"
    :body
    (spacemacs/find-dotfile)
    (persp-add-buffer
     (find-file-noselect "~/.spacemacs.d/private/storax-org/packages.el"))
    (persp-add-buffer ".spacemacs"))

  ;; Builtin Auto Modes
  (add-to-list 'auto-mode-alist '("\\.qss$" . css-mode))
  ;; Aliases
  (defalias 'rs 'replace-string)
  (defalias 'jo 'just-one-space)
  (defalias 'qrr 'query-replace-regexp)
  ;; Vars
  (setq helm-M-x-fuzzy-match nil
        helm-buffers-fuzzy-matching nil
        helm-recentf-fuzzy-match nil
        undo-tree-visualizer-diff nil))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(desktop-auto-save-timeout nil)
 '(erc-nickserv-identify-mode nil)
 '(magit-repository-directories
   (quote
    (("/home/david/projects/" . 1)
     ("/home/david/.homesick/repos/" . 1))))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "mpc" :query "tag:mpc" :key "m"))))
 '(org-agenda-files (quote ("~/Documents/org/todo.org")))
 '(package-selected-packages
   (quote
    (phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode exwm cycle-quotes ox-reveal epc refine counsel prodigy rainbow-mode minimap zenburn-theme yaml-mode xterm-color xkcd ws-butler window-numbering which-key web-mode web-beautify volatile-highlights visual-regexp-steroids vimish-fold vagrant-tramp vagrant use-package toc-org tagedit srefactor spotify spacemacs-theme spaceline smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-yapf pretty-mode popwin pony-mode pip-requirements persp-mode pdf-tools paradox page-break-lines ox-rst ox-jira orgit orgbox org-tfl org-repo-todo org-present org-pomodoro org-plus-contrib org-page org-bullets org-alert open-junk-file neotree multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hyperbole hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-org-rifle helm-notmuch helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag graphviz-dot-mode google-translate golden-ratio gnuplot github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fold-dwim flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emmet-mode elisp-slime-nav electric-operator drag-stuff diff-hl deft define-word cython-mode company-web company-tern company-statistics company-quickhelp company-auctex company-anaconda coffee-mode clean-aindent-mode chruby calfw bundler buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk ansible-doc ansible aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(send-mail-function (quote sendmail-send-it))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:background "#3F3F3F" :foreground "#757565" :inverse-video nil))))
 '(avy-lead-face ((t (:background "#E8BF6A" :foreground "black" :inverse-video nil))))
 '(avy-lead-face-0 ((t (:background "#C45837" :foreground "white" :inverse-video nil))))
 '(avy-lead-face-1 ((t (:background "#6D9CBE" :foreground "black"))))
 '(avy-lead-face-2 ((t (:background "#B4C973" :foreground "black"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(diff-hl-change ((t (:background "#4F4F4F" :foreground "#FF88FF"))))
 '(diff-hl-delete ((t (:background "#4F4F4F" :foreground "#FF5353"))))
 '(diff-hl-insert ((t (:background "#4F4F4F" :foreground "#7FFF7F"))))
 '(font-latex-sectioning-5-face ((t (:inherit fixed-pitch :foreground "#CC9393" :weight bold))))
 '(git-gutter:modified ((t (:background "#DC8CC3" :foreground "#3F3F3F" :weight bold))))
 '(mode-line ((t (:background "#2B2B2B" :foreground "#8FB28F" :box nil :overline "dim gray"))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#5F7F5F" :box nil :overline "dim gray"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#4F4F4F" :foreground "#7F9F7F"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#6F6F6F" :foreground "#9FC29F"))))
 '(region ((t (:background "#222222"))))
 '(scroll-bar ((t (:background "#545450" :foreground "#3F3F3F")))))
