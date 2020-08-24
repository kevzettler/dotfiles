;; Counsel
(use-package counsel
  :ensure t
  :diminish (counsel-mode))

;; Pug mode
(use-package pug-mode
  :mode "\\.pug\\'"
  :config (setq pug-tab-width 2))


;; Sass mode
(use-package sass-mode
  :mode "\\.scss\\'")

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Ivy
(use-package ivy
  :diminish (ivy-mode)
  :bind
  (
   ("C-x b" . ivy-switch-buffer)
   ;;("C-x C-f" . counsel-find-file)
  )
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; Swiper
(use-package swiper
  :bind
  (
;;   ("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   )
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package yaml-mode
  :mode ("\\.yaml" . yaml-mode))

;; Avy
(use-package avy
  :bind ("M-s" . avy-goto-char-timer))

(use-package groovy-mode
  :mode ("Jenkinsfile?" . groovy-mode))

;; projectile
(use-package projectile
  :diminish (projectile-mode)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (counsel-projectile-mode))

;; dumb-jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  :init
  (dumb-jump-mode)
  :ensure
)


;; eslintd-fix
(use-package eslintd-fix)

;; glsl files
(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)))


;; ======== TYPESCRIPT ========
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :config
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t))
  (setq company-tooltip-align-annotations t)
  (setq tide-completion-detailed t)
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-to-list 'company-backends 'company-tide))

;; ======== FLYCHECK ========
(use-package flycheck-flow)
(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq-default flycheck-disabled-checkers '(javascript-jscs html-tidy javascript-standard javascript-jshint))
  :config
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-flow 'web-mode))

  ;; (defun my/use-eslint-from-node-modules ()
  ;;   (let* ((root (locate-dominating-file
  ;;                 (or (buffer-file-name) default-directory)
  ;;                 "node_modules"))
  ;;          (eslint (and root
  ;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
  ;;                                         root))))
  ;;     (when (and eslint (file-executable-p eslint))
  ;;       (setq-local flycheck-javascript-eslint-executable eslint))))

  ;; (defun my/use-flow-from-node-modules ()
  ;;   (let* ((root (locate-dominating-file
  ;;                 (or (buffer-file-name) default-directory)
  ;;                 "node_modules"))
  ;;          (flow (and root
  ;;                     (expand-file-name "node_modules/flow-bin/vendor/flow"
  ;;                                       root))))
  ;;     (when (and flow (file-executable-p flow))
  ;;      (setq-local flycheck-javascript-flow-executable flow))))
  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  ;;(add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules))

(defun tslint-fix-file ()
  (interactive)
  (message "tslint --fixing the file" (buffer-file-name))
  (shell-command (concat "tslint --fix " (buffer-file-name))))

(defun tslint-fix-file-and-revert ()
  (interactive)
  (tslint-fix-file)
  (revert-buffer t t))

;; Nodejs Repl
(use-package nodejs-repl)
(use-package add-node-modules-path)
(use-package web-mode
  :mode (("\\.js[x]?\\'" . web-mode)
         ("\\.ts[x]?\\'" . web-mode))
  :bind (:map web-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-j" . nodejs-repl-send-line)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-l" . nodejs-repl-load-file)
              ("C-c C-z" . nodejs-repl-switch-to-repl))
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("jsx" . "\\.tsx?\\'")))
  (setq
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-enable-auto-quoting nil ;; disable auto quoting
   standard-indent 2
   tab-width 1
   indent-tabs-mode nil
   js-indent-level 2
   js-switch-indent-offset t
   js2-basic-offset 2
   sgml-basic-offset 2
   js2-jsx-mode 2
   js2-highlight-level 3
   js2-indent-level 2
   js2-indent-switch-body t
   js2-strict-semi-warning nil
   js2-missing-semi-one-line-override nil
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   js2-strict-trailing-comma-warning nil)
  (add-hook 'web-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'eslintd-fix-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
                ;; (setq-default flycheck-disabled-checkers '(javascript-jscs html-tidy javascript-standard javascript-jshint javascript-eslint javascript-flow))
                ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
                ;; (flycheck-add-mode 'tsx-tide 'web-mode)
                ;; (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
                ;; (flycheck-add-next-checker 'tsx-tide '(t . typescript-tslint) 'append))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "ts" (file-name-extension buffer-file-name))
                (setup-tide-mode)
                (setq-default flycheck-disabled-checkers '(javascript-jscs html-tidy javascript-standard javascript-jshint javascript-eslint javascript-flow jsx-tide tsx-tide handlebars))
                (flycheck-add-mode 'typescript-tslint 'web-mode)
                (flycheck-add-mode 'javascript-eslint 'web-mode)
                (flycheck-add-mode 'typescript-tide 'web-mode))))
                ;; (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'appdend)
                ;; (flycheck-add-next-checker 'tsx-tide '(t . typescript-tslint) 'append))))
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setq-default flycheck-disabled-checkers '(javascript-jscs html-tidy javascript-standard javascript-jshint tsx-tide typescript-tslint jsx-tide))
              (flycheck-add-next-checker 'javascript-eslint 'javascript-flow 'append))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "js" (file-name-extension buffer-file-name))
                (setq-default flycheck-disabled-checkers '(javascript-jscs html-tidy javascript-standard javascript-jshint tsx-tide typescript-tslint jsx-tide))
                (flycheck-add-mode 'javascript-eslint 'web-mode)
                (flycheck-add-next-checker 'javascript-eslint 'javascript-flow 'append)))))
