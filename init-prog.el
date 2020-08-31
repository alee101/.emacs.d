;;; init-prog.el --- language specific init

;; JS
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (progn
    (setq
     js-indent-level 2
     js2-basic-offset 2
     js2-include-node-externs t
     js2-strict-inconsistent-return-warning nil
     js2-strict-trailing-comma-warning nil)
    ;; (add-hook 'js2-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{121\\}" "hi-yellow")))
    ))

(custom-set-variables
 '(js2-bounce-indent-p t)
 '(js2-indent-switch-body t))

(use-package typescript-mode
  :mode "\\.tsx\\'"
  :config
  (progn
    (setq typescript-indent-level 2)
    ;; (add-hook 'typescript-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{161\\}" 'hi-yellow)))
    ))

(use-package tide
  :ensure t
  :disabled
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . eldoc-mode)
         ;; (before-save . tide-format-before-save)
         ))
;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)
;; (setq tide-format-options '(:indentSize 2 :tabSize 2))

(use-package tern
  :diminish tern-mode
  ;; Run `npm install -g tern' and set to the path where tern is installed
  :init (setenv "PATH" (concat (getenv "PATH") ":/Users/albertlee/.nvm/versions/node/v8.9.0/bin"))
  :defer t
  :after js2-mode
  :config
  (progn
    (add-hook 'js-mode-hook 'tern-mode)))



;; HTML + CSS
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

(use-package less-css-mode
  :mode "\\.css?\\'"
  :defer t
  :config (setq css-indent-offset 2))

(use-package rainbow-mode
  :diminish rainbow-mode
  :defer t
  :after less-css-mode
  :config
  (progn
    (add-hook 'less-css-mode-hook 'rainbow-mode)))



;; Clojure + ClojureScript
(use-package cider)
(use-package clojure-mode
  :defer t
  :config (add-hook 'clojure-mode-hook
                    (lambda ()
                      ;; (aggressive-indent-mode)
                      (paredit-mode)
                      ;; Remove keybinding conflict with ace-jump
                      (local-unset-key (kbd "C-c SPC")))))



;; Python
(setq python-shell-interpreter "python3")



;; Haskell
(use-package haskell-mode)



;; Go
(use-package go-mode
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)))



;; Rust
(use-package rust-mode
  :config
  (progn
    (setq rust-format-on-save t)))



;; Elixir
(use-package elixir-mode)
(use-package erlang)


;; Scala
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))




;; LaTeX
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t))



;; Protobuf
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


;; APIs
(use-package restclient
  :mode ("\\.api\\'" . restclient-mode))



;; Terraform
(use-package terraform-mode
  :config
  (progn
    (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)))



;; C/C++
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(add-hook 'c++-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-c"))))
