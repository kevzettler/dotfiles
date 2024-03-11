;;; package -- Typescript mode setup

;;; Code:
(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . lsp-deferred)
  )
