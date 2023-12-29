;;; init-org.el --- Org & text mode settings

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (progn
    (add-hook 'org-mode-hook
              (lambda ()
                (visual-line-mode t)
                (org-indent-mode t)
                ;; Remove keybinding conflict with avy
                (local-unset-key (kbd "C-c SPC")))
              t)
    (setq
     org-src-fontify-natively t
     org-log-done t
     org-capture-templates `(("t" "Task" entry (file+headline "~/workspace/refile.org" "Tasks")
                              "* TODO %?\n:CAPTURED: %T\n")
                             ("n" "Note" plain (file+headline "~/workspace/refile.org" "Notes")
                              "%?\n:CAPTURED: %T\n" :empty-lines 1)))))


(use-package darkroom
  :config
  (progn
    (setq darkroom-text-scale-increase 1)))


;; Text mode settings
(add-hook 'text-mode-hook
	  (lambda ()
	    (visual-line-mode t)
	    (org-indent-mode t))
	  t)
