(defvar bootstrap-version)
(let ((bootstrap-file
(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
(bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
(with-current-buffer
(url-retrieve-synchronously
"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
'silent 'inhibit-cookies)
(goto-char (point-max))
(eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package restart-emacs
  :straight t
  :commands (restart-emacs))

(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

(use-package bug-hunter
  :straight t)

(use-package general
  :straight t
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))

(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs-tmp"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(add-hook 'prog-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)
                            (setq-default tab-width 2)
                            ))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun add-to-path (path)
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat path ":" (getenv "PATH"))))

(add-to-path "/usr/local/bin/")

(setq vc-follow-symlinks t)

(show-paren-mode)

(transient-mark-mode)

(setq scroll-conservatively 101
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

(setq auto-window-vscroll nil)

(add-hook 'prog-mode-hook 'linum-mode 'smartparens-mode)

(add-hook 'prog-mode-hook 'smartparens-mode)

(set-face-attribute 'default nil :height 130)

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package diminish
  :straight t)

(display-time-mode 1)
(display-battery-mode 1)
(column-number-mode 1)

(use-package smart-mode-line
  :straight t
  :init
  (setq sml/theme nil)
  (add-hook 'after-init-hook 'sml/setup))

(use-package which-key
  :diminish which-key-mode
  :straight t)
(which-key-mode)

(setq which-key-enable-extended-define-key t)

(which-key-mode 1)

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  :after (general)
  :general
  (general-mmap ";" 'evil-ex))

;(use-package evil-surround
;  :straight t
;  :config
;  (global-evil-surround-mode 1))

(use-package smartparens
  :straight t)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-mode-list nil)
  (evil-collection-init 'calc)
  (evil-collection-init 'cider)
  (evil-collection-init 'compile)
  (evil-collection-init 'custom)
  (evil-collection-init 'dired)
  (evil-collection-init 'ediff)
  (evil-collection-init 'elfeed))
  (evil-collection-init 'flycheck)
  (evil-collection-init 'flymake)
  (evil-collection-init 'geiser)
  (evil-collection-init 'helpful)
  (evil-collection-init 'info)
  (evil-collection-init 'profiler)
  (evil-collection-init 'sly)
  (evil-collection-init 'tablist)
  (evil-collection-init 'vterm)
  (evil-collection-init 'xref)

(general-create-definer global-definer
  :keymaps 'override
  :states '(insert emacs normal hybrid motion visual operator)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(defun better-universal-argument ()
  (interactive)
  (if current-prefix-arg
      (universal-argument-more current-prefix-arg)
    (universal-argument)))

(global-definer
  "u" '(better-universal-argument :wk "universal"))

(defmacro general-global-menu-definer (def infix-key &rest body)
  "Create a definer named general-global-DEF wrapping global-definer.
The prefix map is named 'my-DEF-map'."
  (let ((definer-name (intern (concat "general-global-" def))))
    (if (fboundp definer-name)
        `(,definer-name ,@body)
      `(progn
         (general-create-definer ,definer-name
           :wrapping global-definer
           :prefix-map ',(intern (concat "my-" def "-map"))
           :infix ,infix-key
           :wk-full-keys nil
           "" '(:ignore t :which-key ,def))
         (,definer-name
           ,@body)))))

(general-create-definer general-global-motion-definer
  :keymaps 'override
  :states '(normal motion visual operator)
  :prefix "g")

(general-create-definer general-local-motion-definer
  :states 'normal
  :keymaps 'override
  :prefix "g")

(define-key evil-motion-state-map "," nil)

(general-create-definer general-mode-leader-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix ","
  "" '(:ignore t :which-key "mode"))

(general-def evil-normal-state-map "q" 'nil)

(use-package ivy
  :straight t
  :diminish ivy-mode
  :init
  (setq ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-ignore-order)))
  (ivy-mode 1)
  :config
  :general

  (general-mmap "/" 'swiper))

(use-package counsel
  :straight t
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil)
  :general
  ("M-x" 'counsel-M-x)
  (global-definer "SPC" '(counsel-M-x :wk "M-x")))

(use-package hydra
  :straight t)


(use-package ivy-hydra
  :straight t
  :after (ivy hydra))

(global-definer
 "i" '(counsel-imenu :wk "imenu"))

(use-package helpful
  :straight t
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(general-global-menu-definer "help" "h"
                             "i" '(info                      :wk "info")
                             "D" '(toggle-debug-on-error     :wk "toggle debugger"))
(general-global-menu-definer "describe" "h d"
                             "b" '(describe-bindings         :wk "describe bindings")
                             "F" '(counsel-faces             :wk "describe face")
                             "f" '(counsel-describe-function :wk "describe function")
                             "k" '(describe-key              :wk "describe key")
                             "v" '(counsel-describe-variable :wk "describe variable")
                             "m" '(describe-mode             :wk "describe mode")
                             "p" '(describe-package          :wk "describe package")
                             "'" '(describe-char             :wk "describe char"))

(defun open-scratch-buffer ()
  (interactive)
  (display-buffer (get-buffer-create "*scratch*")))

(general-global-menu-definer "buffer" "b"
                             "b" '(counsel-switch-buffer          :wk "switch buffer")
                             "c" '(compile                        :wk "compile")
                             "d" '(kill-current-buffer            :wk "kill buffer")
                             "r" '(rename-buffer                  :wk "rename buffer")
                             "F" '(font-lock-fontify-buffer       :wk "fontify buffer")
                             "N" '(evil-buffer-new                :wk "new buffer")
                             "n" '(next-buffer                    :wk "next-buffer")
                             "p" '(previous-buffer                :wk "previous-buffer")
                             "s" '(open-scratch-buffer            :wk "switch to scratch buffer")
                             "x" '(kill-buffer-and-window         :wk "kill-buffer-and-window"))

(global-definer
  ;"," '(counsel-switch-buffer :wk "switch buffer")
  "x" '(open-scratch-buffer   :wk "scratch buffer"))

(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/readme.org"))

(defun compile-config-file ()
  (interactive)
   (org-babel-tangle-file "~/.emacs.d/readme.org")
   (load-file "~/.emacs.d/init.el"))

(defun open-private-config-file ()
  (interactive)
  (find-file "~/.emacs.d/private.org"))

(defun open-straight-repo ()
  (interactive)
  (counsel-find-file "~/.emacs.d/straight/repos/"))

(defun open-nixos-config ()
  (interactive)
  (find-file "/sudo::/etc/nixos/configuration.nix"))

(defun open-xmonad-config ()
  (interactive)
  (find-file "~/.xmonad/xmonad.hs"))

(defun open-xmobar-config ()
  (interactive)
  (find-file "~/.xmobarrc"))

(general-global-menu-definer "file" "f"
                             "f" '(counsel-find-file        :wk "find file")
                             "r" '(counsel-recentf          :wk "recent files")
                             "s" '(save-buffer              :wk "save buffer")
                             "I" '(open-private-config-file :wk "private config file"))

(global-definer
"." '(counsel-find-file :wk "find file"))

(general-global-menu-definer "configs" "f e"
                             "c" '(open-nixos-config   :wk "configuration.nix")
                             "d" '(open-config-file    :wk "emacs")
                             "R" '(compile-config-file :wk "compile emacs config")
                             "x" '(open-xmonad-config  :wk "xmonad")
                             "X" '(open-xmobar-config  :wk "xmobar"))

(defun open-notebook ()
  (interactive)
  (find-file "~/.org/notebook.gpg"))

(general-global-menu-definer "documents" "f d"
                             "n" '(open-notebook :wk "notebook"))

(use-package ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (general-global-menu-definer "window" "w"
                               "w" '(ace-window :wk "switch") ;; NOTE: You can also use 'SPC u SPC w w'
                               "W" '((lambda () (interactive) (ace-window 4)) :wk "swap")))

(general-global-menu-definer
 "window" "w"
 "c" '(whitespace-cleanup :wk "whitespace cleanup") ;; :(
 "h" '(evil-window-left     :wk "left")
 "j" '(evil-window-down     :wk "down")
 "k" '(evil-window-up       :wk "up")
 "l" '(evil-window-right    :wk "right")
 "v" '(evil-window-vsplit   :wk "vertical split")
 "s" '(evil-window-split    :wk "horizontal split")
 "d" '(evil-window-delete   :wk "close")
 "o" '(delete-other-windows :wk "close other"))

(use-package org
  :straight t)

(add-hook 'org-mode-hook 'auto-fill-mode)

(use-package htmlize
  :straight t)

(use-package ox-pandoc
  :straight t)

;(require 'ox-pandoc)

(require 'org-tempo)

(setq org-startup-folded t)

(general-mode-leader-definer 'org-mode-map
  ;; General
  "c" '(org-ctrl-c-ctrl-c     :wk "update")
  ;; Editing
  "'" '(org-edit-special     :wk "edit")
  "l" '(org-insert-link      :wk "insert link")
  ;; Navigation
  "o" '(org-open-at-point    :wk "open")
  ;; Search
  "." '(counsel-org-goto     :wk "goto")
  "/" '(counsel-org-goto-all :wk "goto all")
  ;; Deadlines
  "s" '(org-schedule         :wk "schedule")
  "d" '(org-deadline         :wk "deadline")
  ;; Tasks
  "t" '(org-todo             :wk "todo")
  "w" '(org-refile           :wk "refile"))

(setq org-edit-src-content-indentation 2)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation nil)

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq org-src-window-setup 'other-window)

(add-to-list 'org-src-lang-modes '("inline-js" . javascript)) ;; js2 if you're fancy

(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))
(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))

(use-package gnuplot
  :straight t)

(advice-add 'org-babel-execute:haskell :override
            (lambda (body params)
              "Execute a block of Haskell code."
              (require 'inf-haskell)
              (add-hook 'inferior-haskell-hook
                        (lambda ()
                          (setq-local comint-prompt-regexp
                                      (concat haskell-prompt-regexp "\\|^λ?> "))))
              (let* ((session (cdr (assq :session params)))
                     (result-type (cdr (assq :result-type params)))
                     (multi-line (string= "yes" (cdr (assq :multi-line params))))
                     (full-body (org-babel-expand-body:generic
                                 body params
                                 (org-babel-variable-assignments:haskell params)))
                     (session (org-babel-haskell-initiate-session session params))
                     (comint-preoutput-filter-functions
                      (cons 'ansi-color-filter-apply comint-preoutput-filter-functions))
                     (raw (org-babel-comint-with-output
                              (session org-babel-haskell-eoe t full-body)
                            (when multi-line
                              (insert ":{")
                              (comint-send-input nil t))
                            (insert (org-trim full-body))
                            (comint-send-input nil t)
                            (when multi-line
                              (insert ":}")
                              (comint-send-input nil t))
                            (insert org-babel-haskell-eoe)
                            (comint-send-input nil t)))
                     (results (mapcar #'org-strip-quotes
                                      (cdr (member org-babel-haskell-eoe
                                                   (reverse (mapcar #'org-trim raw)))))))
                (org-babel-reassemble-table
                 (let ((result
                        (pcase result-type
                          (`output (mapconcat #'identity (reverse (cdr results)) "\n"))
                          (`value (car results)))))
                   (org-babel-result-cond (cdr (assq :result-params params))
                     result (org-babel-script-escape result)))
                 (org-babel-pick-name (cdr (assq :colname-names params))
                                      (cdr (assq :colname-names params)))
                 (org-babel-pick-name (cdr (assq :rowname-names params))
                                      (cdr (assq :rowname-names params)))))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)
   (gnuplot . t)
   (shell . t)
   (sql . t)
   (haskell . t)
   (C       . t)))

(general-global-motion-definer
  "A" '(align-regexp :wk "align"))

(use-package treemacs
  :straight t
  :init
  (treemacs-resize-icons 14))

(use-package projectile
  :straight t)

(use-package counsel-projectile
  :straight t)

(projectile-mode 1)
(counsel-projectile-mode 1)

(global-definer
  "p" '(:keymap projectile-command-map :package projectile :wk "project"))

(general-global-menu-definer
 "search" "/"
 "p" '(projectile-ripgrep :wk "rg"))

(defun counsel-projectile-switch-project-action-open-tab (project)
  "Open a new tab for PROJECT."
  (let ((projectile-switch-project-action
         (lambda ()
           (tab-new)
           (tab-rename project)
           (counsel-projectile-find-file))))
    (counsel-projectile-switch-project-by-name project)))


(ivy-set-actions 'counsel-projectile-switch-project
                 '(("t" counsel-projectile-switch-project-action-open-tab "open in new tab")))

(use-package treemacs-projectile
  :straight t)

(use-package company
  :straight t
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t
        company-idle-delay 0.3
        company-echo-delay 0
        company-dabbrev-downcase nil))

(global-company-mode)

(define-key company-mode-map (kbd "TAB") #'company-indent-or-complete-common)

(use-package company-math
  :straight t
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package flycheck
  :straight t)

(use-package flycheck-posframe
  :straight t
  :hook (flycheck-mode . flycheck-posframe-mode))

(add-to-list 'display-buffer-alist
             '("\\*Flycheck errors.*"
               (display-buffer-below-selected display-buffer-at-bottom)
               (inhibit-same-window . t)
               (window-height . 15)))

(use-package rainbow-delimiters
  :straight t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(use-package rg
  :straight t)

(general-global-menu-definer
 "search" "/"
 "r" '(rg :wk "rg"))

(setq whitespace-style '(face trailing empty tabs))
(global-whitespace-mode)
(diminish 'global-whitespace-mode)

(use-package direnv
  :straight t
  :config (direnv-mode))
(use-package nix-sandbox
  :straight t)

(setq default-nix-wrapper
      (lambda (args)
        (append (list "nix-shell" "--command")
                (list (mapconcat 'identity args " ")))
        (list (nix-current-sandbox))))

(use-package magit
  :straight t)

(use-package evil-magit
  :straight t)

(general-global-menu-definer "git" "g")
(general-global-git
  "b" '(magit-blame  :wk "blame")
  "g" '(magit-status :wk "status")
  "s" '(magit-status :wk "status"))

(use-package git-gutter-fringe
  :straight t
  :diminish git-gutter-mode
  :init
  ;; Hack for org mode
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  (git-gutter-mode)
  :config
  (general-global-git
    "h" '(git-gutter:stage-hunk :wk "stage hunk")))

(use-package git-timemachine
  :straight t
  :init
  (evil-collection-init 'git-timemachine)
  (general-global-git
    "t" '(git-timemachine :wk "timemachine")))

(use-package ediff
  :defer t
  :config
  ;; Disable whitespace checking
  (setq ediff-diff-options "w")
  (setq ediff-split-window-function #'split-window-vertically
        ediff-window-setup-function #'ediff-setup-windows-plain))

(setq epa-pinentry-mode 'loopback)

(general-global-menu-definer "open" "o"
                             "e" '(eshell :wk "eshell"))

(defun eshell-toggle-auto-scroll ()
  (interactive)
  (setq eshell-scroll-to-bottom-on-input (not eshell-scroll-to-bottom-on-input)))

(general-local-motion-definer
 'eshell-mode-map
 "j" 'eshell-next-input
 "k" 'eshell-previous-input)

(defun org-pandoc-html5-filter (contents _backend _info)
  "Convert Org CONTENTS into html5 output."
  (let ((backup-inhibited t)
    contents-filename
    process
    buffer)
    (unwind-protect
    ;; org-pandoc runs pandoc asynchronous.  We need to
    ;; synchronize pandoc for filtering.  `org-pandoc-run' returns
    ;; the process needed for synchronization.  Pityingly we need
    ;; to call `org-pandoc-run-to-buffer-or-file' which handles
    ;; additional options and special hooks.  Therefore we
    ;; temporarily advice `org-pandoc-run' to give us the process.
    (cl-letf* ((original-org-pandoc-run (symbol-function 'org-pandoc-run))
           ((symbol-function 'org-pandoc-run) (lambda (&rest a)
                            (setq process (apply original-org-pandoc-run a)))))
      (setq contents-filename (make-temp-file ".tmp" nil ".org" contents))
      (org-pandoc-run-to-buffer-or-file
       contents-filename
       'html5
       nil ;; not only the sub-tree
       t) ;; buffer
      (while (process-live-p process)
        (sit-for 0.5))
      (with-current-buffer (setq buffer (process-buffer process))
        (buffer-string)))
      (when (file-exists-p contents-filename)
    (delete-file contents-filename))
      (when (buffer-live-p buffer)
    (kill-buffer buffer))
      )))

(org-export-define-derived-backend
    'pandoc-html5
    'pandoc
  :filters-alist '((:filter-final-output . org-pandoc-html5-filter)))

(defun org-pandoc-publish-to-html (plist filename pub-dir)
  "Publish an org file to html using ox-pandoc. Return output file name."
  (let ((org-pandoc-format "html5"))
    (org-publish-org-to
     'pandoc-html5
     filename
     (concat "." (or (plist-get plist :html-extension)
             org-html-extension
             "html"))
     plist
     pub-dir)))

(defun convert-post (src-path)
  (let* ((space " ")
         (name (f-base src-path))
         (target-path (concat "/home/solomon/.org/blog/build/" name "/"))
         (title (concat "--metadata title=\"" (s-titleized-words (substring name 10)) "\""))
         (template "--template=/home/solomon/.org/blog/template.html")
         (cmd (concat "pandoc"
                      space
                      src-path
                      space
                      template
                      space
                      "-f org -t html5 -s -o"
                      space
                      target-path
                      "index.html"
                      space
                      title)))
    (f-mkdir target-path)
    (shell-command cmd)
    ))

(defun build-blog-posts ()
  (interactive)
  (let ((posts (f-entries "~/.org/blog/org")))
    (mapc 'convert-post posts)
    (shell-command "rsync -r --delete ~/.org/blog/build/ cofree.coffee:/srv/www/blog.cofree.coffee")
    ))

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (general-local-motion-definer
   'lsp-mode-map
   "d" 'lsp-find-definition
   "I" 'lsp-find-implementation
   "R" 'lsp-find-references
   "D" 'xref-pop-marker-stack)
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   "K" 'lsp-describe-thing-at-point))

(add-to-list 'display-buffer-alist
             '("\\*lsp-help\\*"
               (display-buffer-below-selected display-buffer-at-bottom)
               (inhibit-same-window . t)
               (window-height . 15)))

(use-package company-lsp
  :straight t
  :commands company-lsp)

(require 'company-lsp)
(push 'company-lsp company-backend)

(defun lsp-toggle-log-io ()
  "Toggle `lsp-log-io'"
  (interactive)
  (if lsp-log-io
      (setq lsp-log-io nil)
    (setq lsp-log-io t))
  (if lsp-print-performance
      (setq lsp-print-performance t)
    (setq lsp-print-performance nil)))

(use-package dap-mode;
  :straight t)

(use-package lsp-treemacs
  :straight t)

(general-define-key
 :keymaps 'prog-mode-map
 "C-(" 'sp-forward-barf-sexp
 "C-)" 'sp-forward-slurp-sexp)

(use-package haskell-mode
  :straight t
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook (lambda () (flycheck-mode -1)))
  :config
  ;(setq haskell-nix-wrapper
  ;      (lambda (args)
  ;        (apply default-nix-wrapper (list (append args (list "--ghc-option" "-Wwarn"))))))

  ;(setq haskell-process-wrapper-function haskell-nix-wrapper)

  (setq haskell-process-wrapper-function
        (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

  (setq haskell-font-lock-symbols t)
  (setq haskell-process-use-presentation-mode t)
  (setq haskell-ghci-options
        '("-ferror-spans"
          "-fdefer-typed-holes"
          "-fno-max-relevant-binds"
          "-fno-diagnostics-show-caret"
          "-fno-show-valid-hole-fits"
          "-fobject-code"
          "-fbyte-code"))
  (setq haskell-process-args-cabal-new-repl
        (mapcar (lambda (opt) (concat "--repl-options=" opt)) haskell-ghci-options))
  (setq haskell-process-args-stack-ghci
        (list (concat "--ghci-options=" (string-join haskell-ghci-options " "))
              "--no-build"
              "--no-load"))
  (setq haskell-process-args-ghci haskell-ghci-options)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-font-lock-symbols-alist
        '(("\\" . "λ")
          ("." "∘" haskell-font-lock-dot-is-not-composition)
          ("forall" . "∀")))
  (setq haskell-interactive-popup-errors nil)

  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-layout-offset 4))
  ;(push '("*Haskell Presentation*" :height 20 :position bottom) popwin:special-display-config))

(defun haskell-interactive-send-command (cmd)
  (haskell-interactive-mode-set-prompt cmd)
  (with-current-buffer (haskell-session-interactive-buffer (haskell-interactive-session))
    (haskell-interactive-handle-expr)))

(defun haskell-interactive-send-region ()
  "Copy the current line to the prompt, overwriting the current prompt."
  (interactive)
  (let ((l (buffer-substring-no-properties (region-beginning)
                                           (region-end))))
    ;; If it looks like the prompt is at the start of the line, chop
    ;; it off.
    (when (and (>= (length l) (length haskell-interactive-prompt))
               (string= (substring l 0 (length haskell-interactive-prompt))
                        haskell-interactive-prompt))
      (setq l (substring l (length haskell-interactive-prompt))))

    (haskell-interactive-send-command l)))

(add-to-path (expand-file-name "~/.cabal/bin/"))
(add-to-path (expand-file-name "~/.local/bin/"))

(general-mode-leader-definer 'haskell-mode-map
  "?" '(haskell-hoogle                :wk "hoogle")
  "s" '(haskell-interactive-switch    :wk "switch to interactive")
  "i" '(haskell-process-do-info       :wk "info")
  "t" '(haskell-mode-show-type-at     :wk "type")
  "l" '(haskell-process-load-file     :wk "load")
  "r" '(haskell-process-reload        :wk "reload")
  "T" '(haskell-session-change-target :wk "change target")
  "R" '(haskell-process-restart       :wk "restart process"))

(general-local-motion-definer 'haskell-mode-map
  "k" '(beginning-of-defun :wk "top of definition")
  "j" '(end-of-defun :wk "bottom of definition")
  "d" '(haskell-mode-goto-loc :wk "goto definition"))

(general-define-key
 :states 'visual
 :keymaps 'haskell-mode-map
 "e" 'haskell-interactive-send-region)

(general-mode-leader-definer 'haskell-interactive-mode-map
  "s" '(haskell-interactive-switch-back :wk "switch to source"))

(general-local-motion-definer
 'haskell-interactive-mode-map
 "j" 'haskell-interactive-mode-prompt-next
 "k" 'haskell-interactive-mode-prompt-previous)

(evil-collection-define-key 'normal 'haskell-presentation-mode-map
  "q" 'quit-window
  "c" 'haskell-presentation-clear)

(use-package idris-mode
  :straight t
  :init
  (add-hook 'idris-mode-hook 'direnv-mode)
  ;(add-to-list 'display-buffer-alist
  ;             '("\\*idris-holes*"
  ;               (display-buffer-below-selected display-buffer-at-bottom)
  ;               (inhibit-same-window . t)
  ;               (window-height . 15)))
  ;(add-to-list 'display-buffer-alist
  ;             '("\\*idris-notes*"
  ;               (display-buffer-below-selected display-buffer-at-bottom)
  ;               (inhibit-same-window . t)
  ;               (window-height . 15)))
  )

(general-mode-leader-definer 'idris-mode-map
  "c" '(idris-case-split              :wk "case split")
  "d" '(idris-add-clause              :wk "add clause")
  "D" '(idris-docs-at-point           :wk "docs at point")
  "l" '(idris-make-lemma              :wk "make lemma")
  "p" '(idris-proof-search            :wk "proof search")
  "r" '(idris-load-file               :wk "load file")
  "t" '(idris-type-at-point           :wk "type at point")
  "T" '(idris-type-search             :wk "type search")
  "w" '(idris-make-with-block         :wk "add with block"))
 (general-def idris-hole-list-mode-map
   "q" 'kill-buffer-and-window)

(use-package agda-input
  :straight (agda-input :type git :host github :repo "agda/agda"
                        :branch "release-2.6.0.1"
                        :files ("src/data/emacs-mode/agda-input.el")))

(use-package agda2-mode
  :straight (agda2-mode :type git :host github
                        :repo "agda/agda"
                        :branch "release-2.6.0.1"
                        :files ("src/data/emacs-mode/*.el"
                                (:exclude "agda-input.el"))))

;(create-file-template ".*.agda$" "cubical-agda-template" 'agda2-mode)

(general-mode-leader-definer 'agda2-mode-map
  "l" '(agda2-load   :wk "load")
  "r" '(agda2-refine :wk "refine"))

(general-local-motion-definer
 'agda2-mode-map
 "j" 'agda2-next-goal
 "k" 'agda2-previous-goal
 "d" 'agda2-goto-definition-keyboard)

(use-package nix-mode
  :straight t
  :init (add-hook 'nix-mode 'direnv-mode))

(use-package yaml-mode
  :straight t)

(use-package go-mode
  :straight t
  )
