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
      '(aggressive-indent
	evil
	neotree
	cider
	ac-slime
	auto-complete
	autopair
	clojure-mode
	clj-refactor
	elpy
	flycheck
	magit
	markdown-mode
	paredit
	powerline
	rainbow-delimiters
	smartparens
	solarized-theme
	web-mode
	yaml-mode))

;;; Install all needed packages
(package-refresh-contents)
(package-install-selected-packages)

;;; 'Require' list of mandatory package imports
(dolist (pkg package-selected-packages)
  (require pkg))

;;; Set the user info correctly
(setq user-full-name "Andy Mender")
(setq user-mail-address "andymenderunix@gmail.com")

;;; Extend ENV setup (placeholder for now) and execution path
(add-to-list 'exec-path "$HOME/bin")

;;; General configuration
(evil-mode 1)
(global-display-line-numbers-mode)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x g") 'magit-status)

;;; Set up Clojure development environment
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)   
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hoom #'clj-refactor-mode)
