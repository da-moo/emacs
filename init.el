;; Basic interface stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(set-default-font "Hack")
(global-display-line-numbers-mode)

;; Mac specific config
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Change font size based on resolution
;; Based on https://gist.github.com/MatthewDarling/8c232b1780126275c3b4
(defun fontify-frame (&optional frame)
  (interactive)
  (let ((target (or frame (window-frame)))))
  (when (display-graphic-p)
    (if (>= (display-pixel-height) 1440)
	(set-face-attribute 'default nil :height 200)
      (set-face-attribute 'default nil :height 160))))

(fontify-frame)
(add-hook 'focus-in-hook 'fontify-frame)

;; Misc
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)
(defalias 'list-buffers 'ibuffer-other-window)

;; Set up packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; Custom packages
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package ace-window
  :ensure t
  :defer 1
  :config
  (global-set-key [remap other-window] 'ace-window)
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (setq aw-background nil)
  (set-face-attribute 'aw-leading-char-face
  nil :foreground "red" :weight 'bold :height 3.0))

(use-package avy
  :ensure t
  :bind ("C-." . avy-goto-char)
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package solarized-theme
  :ensure t)

(use-package oceanic-theme
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package counsel
  :ensure t
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
  :ensure t
  :config (global-company-mode) (setq company-idle-delay t))

(use-package org
  :ensure t
  :bind (("\C-c l" . org-store-link)
	 ("\C-c a" . org-agenda)
	 ("\C-c c" . org-capture)
	 ("\C-c b" . org-iswitchb))
  :config
  (setq org-log-done t))

(use-package nov
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'"
					  . nov-mode)))

(use-package markdown-mode
  :ensure t)

;;;;;;;;;;;;;;;;;
;; Auto-config ;;
;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ns-command-modifier (quote control))
 '(package-selected-packages
   (quote
    (markdown-mode nov zenburn-theme which-key use-package try solarized-theme org oceanic-theme counsel company ace-window))))
