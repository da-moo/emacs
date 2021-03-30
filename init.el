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

;; macOS bootstrap
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Jump to any character on screen (similar to vim-easymotion)
(use-package avy
  :bind ("C-." . avy-goto-char-2)
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

;; Only trim whitespace on changed lines
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Learn keybindings interactively
(use-package which-key
  :config (which-key-mode)
  :custom (which-key-idle-delay 0.3))

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
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :requires lsp-mode
  :commands lsp-ui-mode)

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

;; Visible bell (flash frame)
(setq visible-bell t)

;; Default fill column
(setq-default fill-column 120)

;; Theme
(load-theme 'tango-dark t)

;; Default frame size
(when window-system (set-frame-size (selected-frame) 120 64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes Config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Display visual column guide
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(provide 'init.el)
;;; init.el ends here
