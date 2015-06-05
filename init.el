
;; Packages

(defvar desired-packages)

(setq desired-packages
      '(projectile
	scala-mode2
	sbt-mode
	web-mode
	wgrep
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

;; Generic editing

(setq tab-stop-list (number-sequence 4 120 4))

;; Scala and sbt mode

(setq scala-indent:indent-value-expression t)

(add-hook 'scala-mode-hook 
	  '(lambda ()
	     (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'sbt-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-a") 'comint-bol)
	     (global-set-key (kbd "C-c b") 'sbt-command)))
			      
;; Shell

(add-hook 'shell-mode
	  '(lambda ()
	     (local-set-key (kbd "C-a") 'comint-bol)))

;; UI

(setq inhibit-startup-message t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)

(when (or (> (display-pixel-width) 1400) 
	  (> (window-total-width (selected-window)) 120))
  (split-window-right))

(dired default-directory)

(load-theme 'ample-zen t)

;; Projectile

(projectile-global-mode)

;; Navigation

(ido-mode t)

(setq ido-enable-flex-matching t)
