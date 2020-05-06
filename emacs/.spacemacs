;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((go
      :variables
      gofmt-command "goimports"
      go-format-before-save t
      go-use-golangci-lint t
      godoc-at-point-function 'godoc-gogetdoc)
     docker
     nginx
     csv
     finance
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ansible
     ;;asciidoc
     auto-completion
     better-defaults
     ;;django
     dap
     deft
     emacs-lisp
     (erc :packages (not persp-mode))
     git
     html
     helm
     haskell
     lsp
     java
     (javascript
      :variables
      js2-strict-missing-semi-warning nil
      js2-basic-offset 2
      js-indent-level 2)
     (typescript
      :variables
      typescript-indent-level 2)
     latex
     markdown
     multiple-cursors
     notmuch
     org
     prodigy
     (python :variables
             python-test-runner nil)
     scala
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
     clojure
     storax-icons
     storax-utils
     storax-unkillable-scratch
     storax-avy
     storax-popup
     storax-dabbrev
     storax-fold-dwim
     storax-go
     storax-helm
     storax-helm-icons
     storax-hydra
     storax-smartparens
     storax-flycheck
     storax-prodigy
     storax-python
     storax-org
     storax-latex
     (storax-dash
      :variables
      storax/docsets-to-install
      '("Ansible" "Bash" "Boost" "C" "C++" "CMake" "Chef" "Common_Lisp" "Clojure"
        "Django" "Docker" "ElasticSearch" "Emacs_Lisp" "Flask" "Go" "Jinja"
        "LaTeX" "PostgreSQL" "Python_2" "Python_3" "Scala" "SQLAlchemy"
        "Vagrant" ("Qt_4" . "Qt"))
      storax/user-docsets-to-install
      '("Alembic" "Packer" "PyMel" "Requests" "Sphinx" "Zsh")
      storax/dash-common-docsets
      '("Bash" "Emacs Lisp" "Go" "Python 3"
        "Qt" "Requests" "Zsh"))
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
     storax-i3
     storax-typescript
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(org-tfl rainbow-mode epc ox-ioslide)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(vi-tilde-fringe helm-gitignore org-alert refine org-notmuch)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq user-full-name "David Zuber"
        user-mail-address "zuber.david@gmx.de"
        vs-follow-symlinks t ; When following sysmlinks always go to the destination
        require-final-newline t
        indicate-empty-lines t))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
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
        undo-tree-visualizer-diff nil
        spacemacs-useful-buffers-regexp
        '("\\*\\(ansi-term\\|eshell\\|shell\\|terminal.+\\)\\(-[0-9]+\\)?\\*"
          "\\*scratch\\*"
          "\\*magit:"
          "\\*notmuch")))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(desktop-auto-save-timeout nil)
 '(erc-nickserv-identify-mode nil)
 '(evil-surround-pairs-alist
   (quote
    ((40 "(" . ")")
     (91 "[" . "]")
     (123 "{" . "}")
     (41 "(" . ")")
     (93 "[" . "]")
     (125 "{" . "}")
     (35 "#{" . "}")
     (98 "(" . ")")
     (66 "{" . "}")
     (62 "<" . ">")
     (116 . evil-surround-read-tag)
     (60 . evil-surround-read-tag)
     (102 . evil-surround-function))))
 '(global-evil-surround-mode t)
 '(magit-repository-directories
   (quote
    (("/home/david/projects/" . 1)
     ("/home/david/.homesick/repos/" . 1))))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "mpc" :query "tag:mpc" :key "m")
     (:name "gmx" :query "tag:gmx" :key "g")
     (:name "web" :query "tag:web" :key "w"))))
 '(org-agenda-files (quote ("~/Documents/org/todo.org")))
 '(package-selected-packages
   (quote
    (pacmacs intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode xpm markdown-mode link-hint hide-comnt eyebrowse dumb-jump dired-narrow packed deferred highlight log4e projectile magit-popup hydra f visual-regexp org org-download notmuch live-py-mode evil-ediff dired+ anaconda-mode smartparens evil flycheck company helm helm-core multiple-cursors avy alert magit git-commit async yasnippet inf-ruby haml-mode js2-mode i3wm phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode exwm cycle-quotes ox-reveal epc refine counsel prodigy rainbow-mode minimap zenburn-theme yaml-mode xterm-color xkcd ws-butler window-numbering which-key web-mode web-beautify volatile-highlights visual-regexp-steroids vimish-fold vagrant-tramp vagrant use-package toc-org tagedit srefactor spotify spacemacs-theme spaceline smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-yapf pretty-mode popwin pony-mode pip-requirements persp-mode pdf-tools paradox page-break-lines ox-rst ox-jira orgit orgbox org-tfl org-repo-todo org-present org-pomodoro org-plus-contrib org-page org-bullets org-alert open-junk-file neotree multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hyperbole hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-org-rifle helm-notmuch helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag graphviz-dot-mode google-translate golden-ratio gnuplot github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fold-dwim flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emmet-mode elisp-slime-nav electric-operator drag-stuff diff-hl deft define-word cython-mode company-web company-tern company-statistics company-quickhelp company-auctex company-anaconda coffee-mode clean-aindent-mode chruby calfw bundler buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk ansible-doc ansible aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(send-mail-function (quote sendmail-send-it))
 '(storax/buffers-to-keep
   (quote
    ("\\*Messsage\\*" "\\*scratch\\*" "\\*anaconda-mode\\*" "\\*http")))
 '(vc-follow-symlinks t))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-by-moving-to-trash nil)
 '(desktop-auto-save-timeout nil)
 '(erc-nickserv-identify-mode nil)
 '(evil-surround-pairs-alist
   (quote
    ((40 "(" . ")")
     (91 "[" . "]")
     (123 "{" . "}")
     (41 "(" . ")")
     (93 "[" . "]")
     (125 "{" . "}")
     (35 "#{" . "}")
     (98 "(" . ")")
     (66 "{" . "}")
     (62 "<" . ">")
     (116 . evil-surround-read-tag)
     (60 . evil-surround-read-tag)
     (102 . evil-surround-function))))
 '(global-evil-surround-mode t)
 '(magit-delete-by-moving-to-trash nil)
 '(magit-repository-directories
   (quote
    (("/home/david/projects/" . 1)
     ("/home/david/.homesick/repos/" . 1)
     ("/home/david/go/src/github.com/storax" . 1))))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "mpc" :query "tag:mpc" :key "m")
     (:name "gmx" :query "tag:gmx" :key "g")
     (:name "web" :query "tag:web" :key "w"))))
 '(org-agenda-files (quote ("~/Documents/org/todo.org")))
 '(package-selected-packages
   (quote
    (dap-mode dockerfile-mode vterm jsonnet-mode toml-mode nginx-mode pacmacs intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode xpm markdown-mode link-hint hide-comnt eyebrowse dumb-jump dired-narrow packed deferred highlight log4e projectile magit-popup hydra f visual-regexp org org-download notmuch live-py-mode evil-ediff dired+ anaconda-mode smartparens evil flycheck company helm helm-core multiple-cursors avy alert magit git-commit async yasnippet inf-ruby haml-mode js2-mode i3wm phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode exwm cycle-quotes ox-reveal epc refine counsel prodigy rainbow-mode minimap zenburn-theme yaml-mode xterm-color xkcd ws-butler window-numbering which-key web-mode web-beautify volatile-highlights visual-regexp-steroids vimish-fold vagrant-tramp vagrant use-package toc-org tagedit srefactor spotify spacemacs-theme spaceline smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-yapf pretty-mode popwin pony-mode pip-requirements persp-mode pdf-tools paradox page-break-lines ox-rst ox-jira orgit orgbox org-tfl org-repo-todo org-present org-pomodoro org-plus-contrib org-page org-bullets org-alert open-junk-file neotree multi-term move-text monokai-theme mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hyperbole hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-org-rifle helm-notmuch helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag graphviz-dot-mode google-translate golden-ratio gnuplot github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fold-dwim flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emmet-mode elisp-slime-nav electric-operator drag-stuff diff-hl deft define-word cython-mode company-web company-tern company-statistics company-quickhelp company-auctex company-anaconda coffee-mode clean-aindent-mode chruby calfw bundler buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk ansible-doc ansible aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(send-mail-function (quote sendmail-send-it))
 '(storax/buffers-to-keep
   (quote
    ("\\*Messsage\\*" "\\*scratch\\*" "\\*anaconda-mode\\*" "\\*http")))
 '(vc-follow-symlinks t)
 '(web-mode-attr-indent-offset 2))
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
)
