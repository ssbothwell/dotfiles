(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; TODO: Figure out what this section is:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-agenda-files (quote ("~/journal.org" "~/notebook.org")))
 '(package-selected-packages
   (quote
    (idris-mode dracula-theme doom-themes solarized-theme color-theme-sanityinc-solarized dtrt-indent smartparens intero general flycheck-haskell projectile whitespace-cleanup-mode flycheck which-key validate minions use-package moody solarized-theme haskell-mode evil)))
 '(sp-base-key-bindings (quote sp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Settings ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Disable Toolbar
(tool-bar-mode -1)

;; Disable Menubar
(menu-bar-mode -1)

;; Disable Scrollbar
(scroll-bar-mode -1)

;; Disable Console Bell
(setq visible-bell 1)

;; Global Line Numbers
(global-linum-mode 1)

(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

;; Replace Yes/No with Y/N
(fset 'yes-or-no-p 'y-or-n-p)

;; Soft Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Set default font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Highlight Matching Parens
(show-paren-mode 1)

;; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs.d/file-backup")))

;; TODO: Change this to change the font color instead of adding a glyph.
;; TODO: Trigger this automatically on file save.
;; Annotate TODO comments
(defun annetate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string
          (propertize "A" 'display '(left-fringe right-triangle)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Investigate RSS Readers

;; Solarized Theme
;(use-package solarized-theme
;  :config
;  (load-theme 'solarized-dark t)
;  (let ((line (face-attribute 'mode-line :underline)))
;    (set-face-attribute 'mode-line          nil :overline   line)
;    (set-face-attribute 'mode-line-inactive nil :overline   line)
;    (set-face-attribute 'mode-line-inactive nil :underline  line)
;    (set-face-attribute 'mode-line          nil :box        nil)
;    (set-face-attribute 'mode-line-inactive nil :box        nil)
;    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; Doom Themes
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
)


;; Projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching nil))
  

;; General
(use-package general
  :config
  (general-define-key
   "<f5>" 'load-theme
   "<f6>" 'align-regexp
   "<f7>" 'intero-restart
   "<f8>" 'hoogle
   "C-c c" 'org-capture
   )
  )

;; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :config
  (setq sp-show-pair-from-inside nil)
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    ))

;; Which-Key
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode 1))

;; TODO: Move This into its own file
;; Fira-Code-Mode
(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
          (width (string-width s))
          (prefix ())
          (suffix '(?\s (Br . Br)))
          (n 1))
     (while (< n width)
       (setq prefix (append prefix '(?\s (Br . Bl))))
       (setq n (1+ n)))
     (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

(defconst fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'fira-code-mode)

(define-globalized-minor-mode global-fira-code-mode fira-code-mode
  (lambda () (fira-code-mode 1)))

(global-fira-code-mode 1)

;; EVIL Mode
(use-package evil
  :config
  (evil-mode 1)
  ;;(general-define-key
  ;; :keymaps 'insert
  ;; "C-l" (insert " -> "))
  (define-key evil-normal-state-map
    (kbd "C-k")
    (lambda ()
      (interactive)
      (evil-scroll-up nil)))
  (define-key evil-normal-state-map
    (kbd "C-j")
    (lambda ()
      (interactive)
      (evil-scroll-down nil)))
  (define-key evil-insert-state-map
    (kbd "C-l")
    (lambda ()
      (interactive)
      (insert " -> ")))
  (define-key evil-insert-state-map
    (kbd "C-S-l")
    (lambda ()
      (interactive)
      (insert " => ")))
  (define-key evil-insert-state-map
    (kbd "C-h")
    (lambda ()
      (interactive)
      (insert " <- ")))
  (define-key evil-insert-state-map
    (kbd "C-j")
    (lambda ()
      (interactive)
      (insert " =<< ")))
  (define-key evil-insert-state-map
    (kbd "C-k")
    (lambda ()
      (interactive)
      (insert " >>= "))))
  ;;(setq evil-want-C-u-scroll t)

;; TODO: Figure out how to customize moody, or create my own modeline script.
;; Moody
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Minions
(use-package minions
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "⚙️")
  (setq minions-direct '(flycheck-mode)))

;; InteractivelyDoThings
(use-package ido
  :config
  (setq ido-enable-flex-matching t
    ido-everywhere t)
  (ido-mode t))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; TODO: Figure out how to adjust whitespace highlighting to just highlight spaces/tabs.
;; Whitespace
;;(use-package whitespace
;;  :config
;;  (global-whitespace-mode 1)
;;  (setq whitespace-style 'spaces)
;;  (setq whitespace-style '(face tabs spaces)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Haskell Configuration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Why doesn't this work on startup?
;;(define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
;; Customization related to indentation.

;; Enable Intero
(intero-global-mode 1)

;; Run hlint after Intero
(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
)

;; Company
(use-package company)

;; dtrt-index
(use-package dtrt-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Javascript Configuration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Trigger project specific `yarn lint` scripts on save

;; (defun find-path (target)
;;   "Find nearest path to TARGET in parent directories.
;; e.g. (jyh/find-path '.git') finds the nearest .git directory path"
;;   (let ((root (jyh/find-parent-path target)))
;; (if root (expand-file-name target root) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Configuration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Test out flycheck with python projects
;; TODO: Investigate potential mypy integration
;; TODO: Setup Python indentation rules


;;;;;;;;;;;;;;;;
;;; Org Mode ;;;
;;;;;;;;;;;;;;;;
;; TODO: Install Org Mode
;; TODO: Learn how to use it
;; TODO: Identify additional packages Steve is using related to Org Mode
(use-package org
  :bind ("C-c a" . org-agenda)
)

(setq org-capture-templates
      '(
        ("t" "Ten Step"
        entry (file+headline "~/notebook.org" "Ten Step")
        "* TODO %?\n%u\n** What Happened\n** Affects My\n** My Part\n"
        )
        ("f" "Fears List"
        entry (file+headline "~/notebook.org" "Fears List")
        "* %u\n %?"
        )
        ("g" "Gratitude List"
        entry (file+headline "~/notebook.org" "Gratitude List")
        "* %u\n %?"
        )
        ("j" "Journal Entry"
         entry (file+datetree "~/journal.org")
         "* %? From: %a"
         :empty-lines 1
        )
       )
)

;;; .emacs ends here
