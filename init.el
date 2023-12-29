;;; init.el

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(load "~/.emacs.d/lisp/init-settings")
(load "~/.emacs.d/lisp/init-core")
(load "~/.emacs.d/lisp/init-prog")
(load "~/.emacs.d/lisp/init-org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(restclient yasnippet wgrep-helm undo-tree smartparens s rainbow-delimiters paredit magit helm-swoop helm-projectile helm-git-grep helm-descbinds go-mode gited git-timemachine flycheck diminish darkroom company avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
