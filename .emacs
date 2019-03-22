(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; TODO: Figure out what this section is:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-starter-offset 4)
 '(haskell-indentation-where-post-offset 4)
 '(haskell-indentation-where-pre-offset 4)
 '(package-selected-packages
   (quote
    (intero general flycheck-haskell projectile whitespace-cleanup-mode flycheck which-key validate minions use-package moody pretty-mode solarized-theme haskell-mode evil))))
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

;; Replace Yes/No with Y/N
(fset 'yes-or-no-p 'y-or-n-p)

;; Set default font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Highlight Matching Parens
(show-paren-mode 1)

;; TODO: Change this to change the font color instead of adding a glyph.
;; TODO: Trigger this automatically on file save.
;; Annotate TODO comments
(defun annotate-todo ()
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

;; Solarized Theme
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; General
(use-package general)

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

;; Pretty Mode
(require 'pretty-mode)

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
      (insert " <- "))))
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
  (ido-mode t))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; TODO: Figure out how to adjust whitespace highlighting to just highlight spaces/tabs.
;; Whitespace
(use-package whitespace
  :config
  (global-whitespace-mode 1)
  (setq whitespace-style 'spaces)
  (setq whitespace-style '(face tabs spaces)))


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

;;; .emacs ends here
