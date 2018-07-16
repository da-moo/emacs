;;; init.el --- DaMoo's init.el
;;;
;;; Commentary:
;;; My personal init.el while learning and using Emacs
;;;
;;; Code:

;; Basic interface stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(set-face-attribute 'default nil :font "Hack")
(global-display-line-numbers-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Mac specific config
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq mac-option-modifier 'meta)
(setq ns-command-modifier (quote control))

;; Use homebrew provided GNU ls for dired
(if (eq system-type 'darwin)
    (setq insert-directory-program "/usr/local/bin/gls"))

(defun resize-font-by-resolution ()
  "Change font size based on resolution.
Useful on a laptop where resolutions can change with external monitors.
Based on https://gist.github.com/MatthewDarling/8c232b1780126275c3b4"
  (interactive)
  (when (display-graphic-p)
    (if (>= (display-pixel-height) 1440)
	(set-face-attribute 'default nil :height 200)
      (set-face-attribute 'default nil :height 160))))

(resize-font-by-resolution)
(add-hook 'focus-in-hook 'resize-font-by-resolution)

;; Misc
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)
(defalias 'list-buffers 'ibuffer-other-window)
(require 'tramp)
(setq tramp-default-method "ssh")
(setq indent-tabs-mode nil)
(setq fringes-outside-margins t)

;; Enable whitespace minor mode only for programming modes
;; Taken from: https://emacs.stackexchange.com/a/40624
(require 'whitespace)
(setq whitespace-style '(face trailing lines-tail))
(define-global-minor-mode my-global-whitespace-mode whitespace-mode
  (lambda ()
    (when (derived-mode-p 'prog-mode)
      (whitespace-mode))))
(my-global-whitespace-mode 1)

;; Set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Custom packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 1)
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package try)

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

(use-package which-key
  :config (which-key-mode))

(use-package ace-window
  :defer 1
  :config
  (global-set-key [remap other-window] 'ace-window)
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (setq aw-background nil)
  (set-face-attribute 'aw-leading-char-face
  nil :foreground "red" :weight 'bold :height 3.0))

(use-package avy
  :bind ("C-." . avy-goto-char)
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

;; Extra themes
;; (use-package solarized-theme)
;; (use-package oceanic-theme)

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package counsel
  :bind (("\C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-r")
  'counsel-minibuffer-history))

(use-package company
  :config (global-company-mode) (setq company-idle-delay t))

(use-package org
  :bind (("\C-c l" . org-store-link)
	 ("\C-c a" . org-agenda)
	 ("\C-c c" . org-capture)
	 ("\C-c b" . org-iswitchb))
  :config
  (setq org-log-done t))

(use-package org-journal
  :config
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-dir "~/Nextcloud/org/journal"))

(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'"
					  . nov-mode)))

(use-package markdown-mode)

(use-package flyspell
  :init
  (setq ispell-dictionary "en_US")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-really-hunspell t)
  :hook (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package flyspell-correct
  :after flyspell
  :commands (flyspell-correct-word-generic
	     flyspell-correct-previous-word-generic)
  :bind (:map flyspell-mode-map
	      ("C-;" . flyspell-correct-previous-word-generic)))

(use-package flyspell-correct-ivy
  :commands (flyspell-correct-ivy)
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (set-face-background 'flycheck-posframe-background-face
		       (face-attribute 'secondary-selection :background)))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :config
  (setq counsel-projectile-mode t))

(provide 'init.el)
;;; init.el ends here
