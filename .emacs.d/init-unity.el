;; unity editor integration from :
;; https://eliza.sh/2021-06-01-using-unity-editor-with-emacs.html
;; https://github.com/elizagamedev/unity.el
(add-to-list 'load-path "~/.emacs.d/unity.el/")
(load "unity.el")
(add-hook 'after-init-hook #'unity-build-code-shim)
(add-hook 'after-init-hook #'unity-setup)
;; libray path for mono framework
(setenv "FrameworkPathOverride" "/Library/Frameworks/Mono.framework/Versions/6.12.0/lib/")
(use-package lsp-mode
  :ensure t
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-keymap-prefix "C-c l"))

(use-package csharp-mode
  :ensure t
  :init
  (defun my/csharp-mode-hook ()
    (setq-local lsp-auto-guess-root t)
    (lsp))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook))
