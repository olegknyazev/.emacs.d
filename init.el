
;; Local settings (for proxy, e.g.)

(add-to-list 'load-path "~/.emacs.d/local")

(load "local-before.el" t)

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

(global-set-key (kbd "C-'") 'next-error)
(global-set-key (kbd "C-M-'") 'previous-error)

;; Scala and sbt mode

(setq scala-indent:indent-value-expression t)

(add-hook 'scala-mode-hook 
	  '(lambda ()
	     (local-set-key (kbd "RET") 'newline-and-indent)
	     (local-set-key (kbd "C-c u") 'sbt-find-usages)
	     (local-set-key (kbd "C-c d") 'sbt-find-definitions)))

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

(let ((monitor-pixel-width
       ;; seems that `display-monitor-attributes-list` is added in 24.4
       (if (boundp 'display-monitor-attributes-list) 
	   (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))
	 (display-pixel-width))))
  (when (or (> monitor-pixel-width 1400) 
	    (> (window-total-width (selected-window)) 200))
    (split-window-right)))

(dired default-directory)

(load-theme 'ample-zen t)

;; Projectile

(projectile-global-mode)

;; Navigation

(ido-mode t)

(setq ido-enable-flex-matching t)

;; Local settings (finishing)

(load "local-after.el" t)
