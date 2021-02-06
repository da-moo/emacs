;;; init.el --- mine!
;;;
;;; Commentary:
;;; My personal init.el while learning and using Emacs.
;;;
;;; Code:

;; Font
(set-face-attribute 'default nil :font "Comic Mono")
(set-face-attribute 'default nil :height 120)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch-serif ((t (:family "DejaVu Sans Mono")))))

;; Modes
(fido-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Carry-over from old config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq tramp-default-method "ssh") ;; Connect via /-:<target>:/path/to/file
(setq indent-tabs-mode nil)
(setq require-final-newline t)
;; Remap some keys to more useful shortcuts
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)
(defalias 'list-buffers 'ibuffer-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; straight.el
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

;; use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

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
  :config (which-key-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes Config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Display visual column guide
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(provide 'init.el)
;;; init.el ends here
