;; Basic interface stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)

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

;;(use-package helm
;;  :ensure t)

(use-package org
  :ensure t
  :bind (("\C-c l" . org-store-link)
	 ("\C-c a" . org-agenda)
	 ("\C-c c" . org-capture)
	 ("\C-c b" . org-iswitchb))
  :config
  (setq org-log-done t))

;; Enable IDO everywhere
;;(ido-mode 1)
;;(setq ido-everywhere t)
;;(setq ido-enable-flex-matching t)

;; Theme
(load-theme 'solarized-light t)

;;;;;;;;;;;;;;;;;
;; Auto-config ;;
;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ns-command-modifier (quote control))
 '(package-selected-packages
   (quote
    (company counsel ace-window which-key try use-package solarized-theme oceanic-theme org))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 200 :family "Inconsolata")))))
