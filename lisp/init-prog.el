;;; init-prog.el --- language specific init

(use-package go-mode
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)))


(use-package restclient
  :mode ("\\.api\\'" . restclient-mode))
