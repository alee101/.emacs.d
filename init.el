;;; init.el

;; Setup load path
(add-to-list 'load-path user-emacs-directory)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package)
  (require 'diminish))
(require 'bind-key)

(setq use-package-always-ensure t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-blame-heading ((t (:background "grey25" :foreground "gray50")))))

(load "~/.emacs.d/custom")
(load "~/.emacs.d/init-settings")
(load "~/.emacs.d/init-core")
(load "~/.emacs.d/init-prog")
(load "~/.emacs.d/init-org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-bounce-indent-p t)
 '(js2-indent-switch-body t)
 '(package-selected-packages
   (quote
    (ghub git-commit haskell-mode helm-core tern tide typescript-mode web-mode with-editor gited rainbow-delimiters s paredit smartparens flycheck company yasnippet git-timemachine magit helm-projectile projectile wgrep-helm helm-descbinds helm-git-grep helm-swoop helm undo-tree diminish use-package))))
(put 'dired-find-alternate-file 'disabled nil)
