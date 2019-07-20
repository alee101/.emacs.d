;;; init-core.el --- core packages

(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward)

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :defer t)

(use-package ace-jump-mode
  :defer t
  :bind ("C-c SPC" . ace-jump-mode))

(use-package helm
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (helm-mode t)
          (helm-autoresize-mode t))
  :bind (("C-x b" . helm-mini)
         ;; ("C-x C-b" . helm-buffers-list)
         ("C-x f" . helm-find)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x C-o" . helm-occur)
         ("C-x c SPC" . helm-all-mark-rings)
         ("C-x t" . helm-etags-select)
         ("C-h a" . helm-apropos)
         ("C-h i" . helm-info-emacs)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch))

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    ;; (setq helm-swoop-use-fuzzy-match t)
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

(use-package helm-git-grep
  :bind (("C-c C-g" . helm-git-grep-at-point)
         ("C-c d" . helm-grep-do-git-grep)
         ("C-c g" . helm-git-grep))
  :config (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch))

(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

(use-package wgrep-helm
  :defer t
  :config (setq wgrep-auto-save-buffer t))

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode 1)
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p t") 'toggle-test)
    (setq projectile-completion-system 'helm)
    (add-to-list 'projectile-globally-ignored-files "node-modules")))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (progn
    (helm-projectile-on)
    (define-key projectile-mode-map (kbd "C-c p C-f") 'helm-projectile-find-file-dwim)
    (setq projectile-switch-project-action 'helm-projectile)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c C-b" . magit-blame-addition))
  :config (setq magit-diff-refine-hunk t))

(use-package gited)
(use-package git-timemachine)

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config (setq yas-snippet-dirs '("~/.emacs.d/snippets/")))

(use-package company
  :config
  (progn
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case nil)
    (setq company-idle-delay 0)
    (add-hook 'prog-mode-hook 'company-mode)
    (add-to-list 'company-backends 'company-tern)))

(use-package flycheck
  :diminish flycheck-mode
  :defer t
  :init (global-flycheck-mode))

(use-package smartparens
  :init
  (progn
    (smartparens-global-mode)
    (show-smartparens-global-mode)
    (use-package paredit))
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-k" . paredit-kill)
         ("M-s" . sp-splice-sexp)
         ("M-S" . sp-split-sexp)
         ("M-J" . sp-join-sexp)
         ("M-(" . paredit-wrap-sexp)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square)
         ("M-[" . paredit-wrap-square))
  :config
  (progn
    (smartparens-strict-mode)))

(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package s)


(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "M-j") 'delete-indentation)
(global-set-key (kbd "M-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))
(global-set-key (kbd "C-x E") 'apply-macro-to-region-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'prog-mode-hook 'subword-mode)

(setq projectile-indexing-method 'alien)
