
;; Local settings (for proxy, e.g.)

(add-to-list 'load-path "~/.emacs.d/local")

(load "local-before.el" t)

;; Packages

(defvar desired-packages
  '(projectile

    scala-mode2
    sbt-mode
    web-mode
    cmake-mode
    clojure-mode
    cider
    csharp-mode

    wgrep
    ample-zen-theme
    expand-region
    company
    editorconfig))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; Installing of all the packages from desired-packages list
(let ((refreshed nil))
  (when (not package-archive-contents)
    (package-refresh-contents)
    (setq refreshed t))
  (dolist (p desired-packages)
    (when (not (package-installed-p p))
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install p))))

;; Modes

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(global-auto-revert-mode t)

(add-hook 'after-init-hook 'global-company-mode)

;; Generic editing

(setq tab-stop-list (number-sequence 4 120 4))
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)

(global-set-key (kbd "C-'") 'next-error)
(global-set-key (kbd "C-M-'") 'previous-error)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(defun toggle-comment-on-line ()
  ;; "Comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (next-line))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(fset 'yes-or-no-p 'y-or-n-p)

;; Python

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq python-indent 4)))

;; C++

(c-add-style "my-style"
             '("bsd"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "my-style")))

;; Scala and sbt mode

(setq scala-indent:indent-value-expression t)

(add-hook 'scala-mode-hook 
	  '(lambda ()
	     (local-set-key (kbd "RET") 'newline-and-indent)
	     (local-set-key (kbd "C-c u") 'sbt-find-usages)
	     (local-set-key (kbd "C-c d") 'sbt-find-definitions)))

(add-hook 'sbt-mode-hook
	  '(lambda ()
	     (global-set-key (kbd "C-c b") 'sbt-command)
             (ansi-color-for-comint-mode-on)))

;; Comint

(add-hook 'comint-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-a") 'comint-bol)))
			      
;; Shell

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (define-key shell-mode-map (kbd "TAB") #'company-complete-common)

;; UI

(setq inhibit-startup-message t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq initial-frame-alist '((fullscreen . maximized)))

(let ((monitor-pixel-width
       (if (fboundp 'display-monitor-attributes-list)
	   (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))
	 (display-pixel-width))))
  (when (or (> monitor-pixel-width 1400) 
	    (> (window-total-width (selected-window)) 200))
    (split-window-right)))

(load-theme 'ample-zen t)

(setq-default frame-title-format "%b (%f)")

;; Projectile

(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(projectile-global-mode)

;; Navigation

(ido-mode t)

(setq ido-enable-flex-matching t)

;; Windows

(when (string-equal system-type "windows-nt")
  (progn
    (setq cygwin-bin "c:\\cygwin\\bin")
    (setenv "PATH" (concat cygwin-bin ";"))
    (setq exec-path '(cygwin-bin))))

(setq null-device "/dev/null")

;; Local settings (finishing)

(load "local-after.el" t)
