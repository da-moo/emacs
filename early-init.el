;;; early-init.el --- Early initialization config
;;;
;;; Commentary:
;;; My early initialization configuration
;;;
;;; Code:

;; Prevent Emacs from blinding us on startup
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))

;; If we're using native-compilation, silence warnings
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
       (setq comp-async-report-warnings-errors nil))

;; Improve startup performance by reducing garbage collection
(setq gc-cons-threshold (* 500 1000 1000))

;; Disable package.el in favor of straight.el
;; (setq package-enable-at-startup nil)

(provide 'early-init.el)
;;; early-init.el ends here
