;;; init-settings.el --- basic settings and setup

(setq inhibit-startup-message t)
(setq debug-on-error t)
(setq-default word-wrap t)
(setq-default insert-tabs-mode -1)
(setq-default indent-tabs-mode nil)
(column-number-mode t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(global-auto-revert-mode t)

;; Save whatever’s in the current (system) clipboard before replacing it with the Emacs’ text.
(setq
 save-interprogram-paste-before-kill t
 mouse-drag-copy-region t)

;; Backups
(setq
 backup-directory-alist `(("." . "~/.emacs.d/backup"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 5
 kept-old-versions 1
 version-control t)

;; History
(setq
 savehist-file "~/.emacs.d/savehist"
 savehist-mode 1
 history-delete-duplicates t
 savehist-save-minibuffer-history 1
 savehist-additional-variables '(kill-ring
                                 search-ring
                                 regexp-search-ring))

;; Appearance
(set-face-attribute 'default nil
                    :family "Menlo" :height 180 :weight 'normal)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Follow symlinks
(setq find-file-visit-truename t)

(put 'dired-find-alternate-file 'disabled nil)

;; Mac-specific rebinding
(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-right-option-modifier 'none
      ns-right-option-modifier  'none)

(require 'bind-key)
