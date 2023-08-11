;;; package -- Typescript mode setup

;;; Code:
(use-package typescript-mode
  :ensure t
  )

;; ======== TYPESCRIPT ========
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :after (web-mode typescript-mode company flycheck)
  :config
  (message "tide setup!!!!!")
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t))
  (setq company-tooltip-align-annotations t)
  ;; aligns annotation to the right hand side
  (setq tide-completion-detailed t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)
  (add-to-list 'company-backends 'company-tide))
