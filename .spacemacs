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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(clojure
     react
     rust
     ocaml
     html
     yaml
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;auto-completion
     ;; better-defaults
     ;(elfeed :variables rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed1.org"
     ;                                              "~/.emacs.d/private/elfeed2.org"))
     emacs-lisp
     git
     helm
     (haskell :variables haskell-process-type 'cabal-new-repl haskell-completion-backend 'ghci)
     idris
     (javascript :variables tern-command '("~/.local/bin/tern"))
     lsp
     ;; markdown
     nixos
     org
     ;org-roam
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;spell-checking
     syntax-checking
     themes-megapack
     typescript
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     nix-sandbox
     direnv
     xclip
   )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

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
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
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
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(sanityinc-tomorrow-eighties
                         monochrome
                         spacemacs-dark
                         spacemacs-light
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-mode-line-theme 'spacemacs
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
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
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (xclip-mode 1)

  (setq idris-interpreter-path "idris")
  (setq idris-load-packages "contrib base prelude")
  (add-hook 'idris-mode-hook 'direnv-mode)

  ;;; Clojure ;;;

  (add-hook 'clojure-mode-hook 'direnv-mode)
  (setq cider-clojure-cli-global-options "-C:test")

  ;;; Haskell Mode

  (setq default-nix-wrapper
        (lambda (args)
          (append
           ;; Change this to match your home directory/preferences
           (append (list "nix-shell" "--command" )
                   (list (mapconcat 'identity args " "))
                   )
           (list (nix-current-sandbox))
           )
          )
        )

  (setq haskell-nix-wrapper
        (lambda (args)
          (apply default-nix-wrapper (list (append args (list "--ghc-option" "-Wwarn"))))
          )
        )

  ;; Haskell repl session that runs in the background
  (setq haskell-process-wrapper-function haskell-nix-wrapper)

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (add-hook 'haskell-mode-hook (lambda () (flycheck-mode -1)))

  (setq haskell-process-use-presentation-mode t)

  (setq haskell-ghci-options
        '("-ferror-spans"
          "-fdefer-typed-holes"
          "-fno-max-relevant-binds"
          "-fno-diagnostics-show-caret"
          "-fno-show-valid-hole-fits"
          "-fobject-code"
          "-fbyte-code"))

  (setq haskell-process-args-stack-ghci
        (list (concat "--ghci-options=" (string-join haskell-ghci-options " "))
              "--no-build"
              "--no-load"))

  (setq haskell-process-args-cabal-new-repl
        (mapcar (lambda (opt) (concat "--repl-options=" opt)) haskell-ghci-options))

  (setq haskell-interactive-popup-errors nil)

  (push '("*Haskell Presentation*" :height 20 :position bottom) popwin:special-display-config)

  (setq haskell-process-args-ghci haskell-ghci-options)


  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil)

  ;; Javascript
  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  ;; Keybindings
  (add-hook 'haskell-mode-hook
            (lambda ()  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
                          "ht" 'haskell-mode-show-type-at))
            99)
  (evil-define-key 'normal haskell-mode-map
    (kbd "C-k")
    (lambda ()
      (interactive)
      (evil-scroll-up nil)))
  (evil-define-key 'normal haskell-mode-map
    (kbd "C-j")
    (lambda ()
      (interactive)
      (evil-scroll-down nil)))
  (evil-define-key 'insert haskell-mode-map
    (kbd "C-l")
    (lambda ()
      (interactive)
      (insert " -> ")))
  (evil-define-key 'insert haskell-mode-map
    (kbd "C-S-l")
    (lambda ()
      (interactive)
      (insert " => ")))
  (evil-define-key 'insert haskell-mode-map
    (kbd "C-h")
    (lambda ()
      (interactive)
      (insert " <- ")))
  (evil-define-key 'insert haskell-mode-map
    (kbd "C-j")
    (lambda ()
      (interactive)
      (insert " =<< ")))
  (evil-define-key 'insert haskell-mode-map
    (kbd "C-k")
    (lambda ()
      (interactive)
      (insert " >>= ")))
  (evil-define-key 'insert haskell-mode-map
    (kbd "C-S-k")
    (lambda ()
      (interactive)
      (insert " >=> ")))
  (evil-define-key 'insert haskell-mode-map
    (kbd "C-S-j")
    (lambda ()
      (interactive)
      (insert " <=< ")))

  (define-key evil-insert-state-map
    (kbd "C-w")
    'other-window)

  ;; Org Mode
  (setq org-todo-keywords
        '((sequence "BACKLOG(b)" "TODO(t)" "|" "DONE(d)")
          (sequence "INCOMPLETE(I)" "|" "COMPLETE(c)")))

  (setq org-todo-keyword-faces
        '( ("BACKLOG" . "gray")
          ))

  (setq org-agenda-custom-commands
        '(("A" "Active Agenda"
           ((agenda "" ((org-agenda-span 1)))
            (tags "style=\"habit\""
                  ((org-agenda-overriding-header "Habits:")))
            (tags "todo=\"TODO\"&-style=\"habit\""
                  ((org-agenda-overriding-header "Active TODOs:"))))
           ((org-agenda-compact-blocks t)))
          ("B" "Backlog"
           ((todo "BACKLOG"
                  ((org-agenda-overriding-header "Backlogged TODOs:")))))
          ))

  ;(setq org-habit-show-all-today t)
  (setq org-habit-show-habits-only-for-today nil)

  ;(add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Org Mode Syntax Highglighting
              (require 'ox-latex)
              ;(add-to-list 'org-latex-packages-alist '("" "minted"))
              (setq org-latex-listings 'minted) 

              (setq org-latex-pdf-process
                    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

              (setq org-src-fontify-natively t)

              ;; Do I need this?
              (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags-command))))

  (setq org-default-notes-file (concat org-directory "~/.org/journal.gpg"))

  (setq org-capture-templates
        '(
          ("T" "Ten Step"
           entry (file+headline "~/.org/aa.gpg" "Ten Step")
           "* %?\n%u\n** What Happened\n** Affects My\n** My Part\n"
           )
          ("f" "Fears List"
           entry (file+headline "~/.org/aa.gpg" "Fears List")
           "* %u\n %?"
           )
          ("g" "Gratitude List"
           entry (file+headline "~/.org/aa.gpg" "Gratitude List")
           "* %u\n %?"
           )
          ("j" "Journal Entry"
           entry (file+datetree "~/.org/journal.gpg")
           "* %?"
           :empty-lines 1
           )
          ("t"
           "Todo"
           entry
           (file+headline "~/.org/notebook.gpg" "Tasks")
           "** BACKLOG %^{Todo} %^G \n:PROPERTIES:\nCreated: %U\n:END:\n%? "
           :prepend t
           :created t
           ))
        )

  (defun drop-til (char xs)
    (if (= char (car xs))
        (cdr xs)
        (dropTil char (cdr xs))))

  (defun words (str)
    (remove "" (split-string str " ")))

  (defun unwords (xs)
    (apply #'concatenate 'string xs))

  (defun lines (str)
    (remove "" (split-string str "\n")))

  (defun decolorize (string)
    (with-temp-buffer
      (insert string)
      (ansi-color-apply-on-region (point-min) (point-max))
      (buffer-string)))

  (defun split-nix-query-result (result)
    (remove nil (cl-loop for it on (lines result) by #'cddr collect (cadr it)))
    )

  ;(defun nix-install-package ()
  ;  (interactive)
  ;  (let* ((query (read-from-minibuffer "Nix Search: "))
  ;         (command (format "nix-env -qaP --description \".*%s.*\" | tee" query))
  ;         (output-buffer (get-buffer-create "*Nix Search*"))
  ;         (proc (start-process-shell-command "Nix Search" output-buffer command)))
  ;    (if (process-live-p proc)
  ;        (set-process-sentinel proc #'nix-search-sentinel)
  ;      (message "No process running."))))

  ;; Nix Sources
  (defun nix-search ()
    (let* ((query (read-from-minibuffer "Nix Search: "))
           (results (decolorize (shell-command-to-string (format "nix search %s" query))))
          (regex "^\\* \\(.*\\) (\\(.*\\))\n[\s]*\\(.*\\)$"))
      (-map (lambda (x) (s-join " - " (cdr x))) (s-match-strings-all regex results))
      ))

  (defun nix-list-packages ()
    (mapcar (lambda (str) (substring str 5))
            (split-nix-query-result (shell-command-to-string "nix-env -qs | tee"))))

  (defun nix-collect-garbage ()
    (interactive)
    (async-shell-command "nix-collect-garbage"))
  (defun nix-uninstall-package (package)
    (async-shell-command (format "nix-env --uninstall %s" package)))

  (defcustom helm-nix-list-installed-actions
    '(("nix-env --uninstall" . nix-uninstall-package))
    "Actions on a list of nix packages"
    :group 'helm
    :type '(alist :key-type string :value-type function))

  (defun helm-nix-list-installed-packages ()
    (interactive)
    (helm :sources (helm-build-sync-source "nix-packages"
                     :action helm-nix-list-installed-actions
                     :candidates (nix-list-packages))
          :buffer "*Nix Installed Package*"
          ))

  (defun helm-nix-search-packages ()
    (interactive)
    (let ((command "nix-env -qa --description | tee"))
      (helm :sources (helm-build-sync-source "nix-packages"
                       :candidates (nix-search)
                       )
            :buffer "*helm nix search*")))
  )

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
 '(haskell-tags-on-save t)
 '(org-agenda-files (quote ("~/.org/notebook.gpg" "~/.org/journal.gpg")))
 '(package-selected-packages
   (quote
    (xclip web-mode utop tuareg caml toml-mode tagedit slim-mode scss-mode sass-mode rjsx-mode racer pug-mode ocp-indent impatient-mode helm-css-scss haml-mode flycheck-rust flycheck-ocaml merlin emmet-mode dune company-web web-completion-data clojure-snippets cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a cargo rust-mode lsp-haskell nix-sandbox direnv org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-cliplink org-brain org-ql transient peg ov org-super-agenda map dash-functional ts htmlize helm-org-rifle helm-org gnuplot evil-org ws-butler writeroom-mode visual-fill-column winum volatile-highlights vi-tilde-fringe uuidgen treemacs-projectile treemacs-persp treemacs-evil treemacs ht pfuture toc-org symon symbol-overlay string-inflection spaceline-all-the-icons all-the-icons memoize spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode password-generator paradox spinner overseer org-bullets open-junk-file nameless move-text macrostep lorem-ipsum link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-xref helm-themes helm-swoop helm-purpose window-purpose imenu-list helm-projectile projectile helm-mode-manager helm-make helm-ls-git helm-flx helm-descbinds helm-ag google-translate golden-ratio flycheck-package package-lint flycheck pkg-info epl let-alist flycheck-elsa flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state iedit evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens smartparens paredit evil-args evil-anzu anzu eval-sexp-fu elisp-slime-nav editorconfig dumb-jump f dash s devdocs define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile packed aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup which-key use-package pcre2el org-plus-contrib hydra lv hybrid-mode font-lock+ evil goto-chg undo-tree dotenv-mode diminish bind-map bind-key async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
)
