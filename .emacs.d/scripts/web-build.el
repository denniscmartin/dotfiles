(load "~/.emacs.d/init.el")

(setq org-file-path (elt argv 0))

(defun org-to-hugo-md ()
  (find-file org-file-path)
  (org-hugo-export-to-md)
  (kill-buffer))

(org-to-hugo-md)

