(require 'package)

;; Set package.el repositories
(setq package-archives
      '(
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	))

(package-initialize)

;; Update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; A list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; Programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

;; START: THIRD-PARTY PACKAGES
;; Get env variables from shell
(use-package exec-path-from-shell
  :ensure t)

;; Set max width for text in visual-line-mode
(use-package visual-fill-column
  :ensure t)

;; Git
(use-package magit
  :ensure t)

;; Mini-buffer completion
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Syntax highlighting for html export
(use-package htmlize
  :ensure t)

;; Languages
(use-package haskell-mode
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package swift-mode
  :ensure t)

(use-package json-mode
  :ensure t)
;; END: THIRD-PARTY PACKAGES

;; START: THEME
(load-theme 'adwaita t)
;; END: THEME

;; START: INTERNAL CONFIG
;; Set global backups directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
;; END: INTERNAL CONFIG

;; START: EDITOR SETTINGS
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Change font size
(set-face-attribute 'default nil :height 130)

;; Show row numbers
(global-display-line-numbers-mode t)

;; Use ibuffer
(global-set-key [remap list-buffers] 'ibuffer)

;; Set timezone to UTC
(setenv "TZ" "UTC0")

;; Set export methods
(setq org-export-backends '(iCalendar ascii html md otc))

;; Enable windmove (easy way to switch windows)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq c-default-style "linux")
;; END: EDITOR SETTINGS

;; START: ORG MODE
(require 'org)

;; Active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)))

;; Automatically evaluate code blocks without asking
(setq org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook
	  (lambda ()

	    ;; Enable soft wrapping
	    (visual-line-mode 1)
	    (visual-fill-column-mode 1)

	    ;; Set wrapping width
	    (setq visual-fill-column-width 90)

	    ;; Disable row numbers
	    (display-line-numbers-mode 0)

	    ;; Add a time-stamp evy time a file is saved
	    ;; if 'Last update: ~ ~' exists
	    (add-hook 'before-save-hook 'time-stamp)

	    ;; Change default time stamp format
	    (setq time-stamp-pattern
		  "-10/Last update:[ \t]+\\\\?[\"~]+%:y-%02m-%02d %02H:%02M:%02S %#Z\\\\?[\"~]")))

(require 'ox-publish)

(setq org-publish-project-alist
      '(
	("denniscm.com"
	 :recursive t
	 :base-directory "~/source/denniscm.com/content"
	 :publishing-directory "~/source/denniscm.com/public"
	 :publishing-function org-html-publish-to-html)))

(defun denniscm-publish-static ()
  (interactive)
  (shell-command "cp -r ~/source/denniscm.com/static ~/source/denniscm.com/public")
  (shell-command "cp ~/source/denniscm.com/src/style.css ~/source/denniscm.com/public"))

(defun denniscm-deploy ()
  (interactive)
  (if (y-or-n-p "Do you added `static` and `style.css` to public?: ")
      (progn
        (shell-command
         "rsync -av --delete ~/source/denniscm.com/public/ \
          dennis@64.226.124.37:/var/www/denniscm.com/html")
        (message "Deployment completed."))
    (message "Deployment cancelled.")))
;; END: ORG MODE 

;; START: CUSTOM TEMPLATES
(setq org-capture-templates
      '(
	("r" "Add repository links" plain (file buffer-name)
	 (file "~/.emacs.d/templates/repo-links.org")
	 :empty-lines 1)
	("w" "Web templates")
	("wb" "Create new blog" plain (file (lambda () (w-blog-filename-create)))
	 (file "~/.emacs.d/templates/web-blog.org"))
	("wl" "Create new log" plain (file (lambda () (w-log-filename-create)))
	 (file "~/.emacs.d/templates/web-log.org"))
	("wk" "Create new knowledge" plain (file (lambda () (w-knwl-filename-create)))
	 (file "~/.emacs.d/templates/web-knwl.org"))
	("wp" "Create new project" plain (file (lambda () (w-proj-filename-create)))
	 (file "~/.emacs.d/templates/web-proj.org"))))

(defun w-blog-filename-create ()
  (let* ((base-directory "~/source/denniscm.com/content/blog/")
	 (date-timestamp (format-time-string "%Y-%m-%d"))
         (blog-file-name (read-string "blog-filename: "))
	 (blog-file-path (expand-file-name
			  (format "%s-%s.org" date-timestamp blog-file-name) base-directory)))
    (if (file-exists-p blog-file-path)
        (error "Blog file '%s' already exists" blog-file-path)
      blog-file-path)))

(defun w-log-filename-create ()
  (let* ((base-directory "~/source/denniscm.com/content/logs/")
	 (myuuid (substring (shell-command-to-string "uuidgen | tr -d - | cut -c 1-8") 0 -1))
	 (diary-file-path (expand-file-name
			  (format "%s.org" myuuid) base-directory)))
    (if (file-exists-p diary-file-path)
        (error "Diary file '%s' already exists" diary-file-path)
      diary-file-path)))

(defun w-knwl-filename-create ()
  (let* ((base-directory "~/source/denniscm.com/content/knwl/")
	 (note-file-name (read-string "note-filename: "))
	 (note-file-path (expand-file-name
			  (format "%s.org" note-file-name) base-directory)))
    (if (file-exists-p note-file-path)
	(error "Note file '%s' already exists" note-file-path)
      note-file-path)))

(defun w-proj-filename-create ()
  (let* ((base-directory "~/source/denniscm.com/content/proj/")
	 (project-file-name (read-string "repo-name: "))
	 (project-file-path (expand-file-name
			  (format "%s.org" project-file-name) base-directory)))
    (if (file-exists-p project-file-path)
	(error "Project file '%s' already exists" project-file-path)
      project-file-path)))
;; END: CUSTOM TEMPLATES

