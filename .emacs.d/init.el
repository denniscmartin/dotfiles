(require 'package)

;; set package.el repositories
(setq package-archives
      '(
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	))

(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)
(use-package exec-path-from-shell
  :ensure t)

(use-package visual-fill-column
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

;; THEME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(adwaita wombat))
 '(package-selected-packages
   '(ox-hugo exec-path-from-shell haskell-mode yaml-pro yaml yaml\.el visual-fill-column use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; GENERAL

;; set global backups directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; EDITOR SETTINGS
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; change font size
(set-face-attribute 'default nil :height 130)

(global-display-line-numbers-mode t)

;; use ibuffer
(global-set-key [remap list-buffers] 'ibuffer)

;; enable windmove (easy way to switch windows)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; CODE STYLE
(setq c-default-style "linux")

;; ORG MODE
(require 'org)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

;; enable soft wrapping
;; set wrapping width
;; disable row numbers
(add-hook 'org-mode-hook
	  (lambda ()
	    (visual-line-mode 1)
	    (visual-fill-column-mode 1)
	    (setq visual-fill-column-width 90)
	    (display-line-numbers-mode 0)))


