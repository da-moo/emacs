;;; init.el --- mine!
;;;
;;; Commentary:
;;; My personal init.el while learning and using Emacs.
;;;
;;; Code:

;; Font
(set-face-attribute 'default nil :font "Comic Mono")
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'fixed-pitch-serif nil :font "DejaVu Sans Mono")

;; Misc display
(column-number-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Carry-over from old config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default tramp-default-method "ssh") ;; Connect via /-:<target>:/path/to/file
(setq indent-tabs-mode nil)
(setq require-final-newline t)
;; Remap some keys to more useful shortcuts
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)
(defalias 'list-buffers 'ibuffer-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-interval 1)
  :config
  (auto-package-update-maybe))

;; macOS bootstrap
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Use homebrew provided GNU ls for dired
(if (eq system-type 'darwin)
    (progn (setq insert-directory-program "/usr/local/bin/gls")
	   (setq ns-command-modifier 'meta)))

;; Learn keybindings interactively
(use-package which-key
  :config (which-key-mode)
  :custom (which-key-idle-delay 0.3))

(use-package god-mode
  :custom
  (god-mode-enable-function-key-translation nil)
  :bind (("<escape>" . #'god-local-mode)
	 ("<end>" . #'god-local-mode)
	 :map god-local-mode-map
	 ("z" . #'repeat)
	 ("i" . #'god-local-mode))
  :config
  (god-mode-all)
  (which-key-enable-god-mode-support))

;; ivy, counsel, & swiper
(use-package counsel
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x b" . counsel-ibuffer)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :custom (ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :custom (ivy-rich-path-style 'abbrev)
  :requires ivy
  :config (ivy-rich-mode 1))

;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Jump to any character on screen (similar to vim-easymotion)
(use-package avy
  :bind ("C-." . avy-goto-char-2)
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

;; Only trim whitespace on changed lines
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Spell check using aspell
(use-package flyspell
  :hook (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :config
  (setq ispell-dictionary "en_US"
	ispell-program-name (executable-find "aspell")
	ispell-really-aspell t
	flyspell-delay 0.25)
  (unbind-key "C-." flyspell-mode-map))

(use-package flyspell-correct
  :requires flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :requires flyspell-correct)

;; Flycheck for linting
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((ruby-mode . lsp)
	 (php-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :requires lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :requires lsp-mode
  :commands lsp-ivy-workspace-symbol)

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g" . magit-status))

(use-package org
  :ensure org-plus-contrib
  :pin org)

(use-package ripgrep)

(use-package projectile
  :init (projectile-mode +1)
  :custom (projectile-sort-order 'recently-active)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package php-mode)

(use-package ruby-mode)

(use-package web-mode
  :custom (web-mode-markup-indent-offset 2)
	  (web-mode-code-indent-offset 2)
  :config (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;; Visual tweaks
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package modus-themes
  :pin melpa
  :init
  (setq modus-themes-bold-constructs t
	modus-themes-slanted-constructs t
	modus-themes-syntax 'alt-syntax
	modus-themes-region 'bg-only
	modus-themes-diffs nil)
  (modus-themes-load-themes)
  :bind ("<f5>" . modus-themes-toggle)
  :config (modus-themes-load-vivendi))

(use-package mode-line-bell
  :config (mode-line-bell-mode))

;;;;;;;;;;;;;;;
;; Key binds ;;
;;;;;;;;;;;;;;;

(bind-key "C-'" 'comment-line)

;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;

;; Replace DAbbrev with Hippie Expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Change default dired switches
(setq dired-listing-switches "-alh --group-directories-first")

;; Default fill column
(setq-default fill-column 120)

;; Default frame size
(when window-system (set-frame-size (selected-frame) 120 64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes Config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Display visual column guide
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(load custom-file)

;; Restore garbage collector settings for runtime
(setq gc-cons-threshold (* 20 1000 1000))

(provide 'init.el)
;;; init.el ends here
