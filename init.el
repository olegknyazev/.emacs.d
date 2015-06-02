
;; Packages

(defvar desired-packages
  '(projectile
    scala-mode2
    web-mode
    ample-zen-theme))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p desired-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Modes

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(global-auto-revert-mode t)

;; UI

(setq inhibit-startup-message t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

; Doesn't works yet:
;; (when (> (window-total-width (selected-window)) 120)
;;   (split-window-right))

(load-theme 'ample-zen t)

;; Projectile

(projectile-global-mode)

;; Navigation

(ido-mode t)

(setq ido-enable-flex-matching t)
