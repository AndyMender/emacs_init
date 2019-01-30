;;; Import CommonLisp functions and macros to extend Emacs
(require 'cl-lib)

;;; Load package management facilities
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;;; Define mandatory packages
(setq package-selected-packages
      '(;; general packages
	neotree
	evil
	auto-complete
	flycheck
	aggressive-indent
	magit
	powerline
	autopair
	markdown-mode
	web-mode
	yaml-mode
	;; themes
	color-theme
	solarized-theme
	material-theme
	;; python
	elpy
	py-autopep8
	jedi
	;; clojure
	cider
	ac-slime
	clojure-mode
	clj-refactor
	paredit
	rainbow-delimiters
	smartparens))

;;; Refresh local package list cache
(unless package-archive-contents
  (package-refresh-contents))

;;; Install all needed packages
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; Load all installed packages
(dolist (pkg package-selected-packages)
  (require pkg))

;;; General configuration
(add-to-list 'exec-path "$HOME/bin")

(setq user-full-name "Andy Mender"
      user-mail-address "andymenderunix@gmail.com"
      inhibit-startup-message t)

(load-theme 'material t)
(evil-mode t)
(global-linum-mode t)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;; Set up Clojure development environment
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)   
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'clj-refactor-mode)

;;; Set up Python development environment
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'elpy-mode-hook #'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook  #'flycheck-mode)
(add-hook 'python-mode-hook #'jedi-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
    (jedi material-theme aggressive-indent evil neotree cider ac-slime auto-complete autopair clojure-mode clj-refactor elpy flycheck magit markdown-mode paredit powerline rainbow-delimiters smartparens solarized-theme web-mode yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
