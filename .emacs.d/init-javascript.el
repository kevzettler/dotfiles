;; Counsel
(use-package counsel
  :ensure t
  :diminish (counsel-mode))

;; Sass mode
(use-package sass-mode
  :mode "\\.scss\\'")

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Ivy
(use-package ivy
  :ensure t
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
;; https://github.com/abo-abo/swiper
(use-package swiper
  :ensure t
  :bind
  (
;;   ("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   )
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package amx
  :ensure t
  :bind (("M-x" . amx))
  :config
  (setq amx-backend 'ivy))

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
  :bind (("C-c d o" . dumb-jump-go-other-window)
         ("C-c d j" . dumb-jump-go)
         ("C-c d x" . dumb-jump-go-prefer-external)
         ("C-c d z" . dumb-jump-go-prefer-external-other-window))
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


;; ======== FLYCHECK ========
(use-package flycheck
  :ensure t
  :diminish (flycheck-mode)
  :init (global-flycheck-mode))

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
         ("\\.ts[x]?\\'" . web-mode)
         )
  :bind (:map web-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-j" . nodejs-repl-send-line)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-l" . nodejs-repl-load-file)
              ("C-c C-z" . nodejs-repl-switch-to-repl))
  :config
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
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'eslintd-fix-mode)
  (add-hook 'web-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setq-default flycheck-disabled-checkers '(javascript-jscs html-tidy javascript-standard javascript-jshint tsx-tide typescript-tslint jsx-tide))
              (flycheck-add-next-checker 'javascript-eslint 'javascript-flow 'append))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "js" (file-name-extension buffer-file-name))
                (setup-tide-mode)
                (message "web-mode javascript action")
                (flycheck-add-mode 'typescript-tide 'web-mode)
                (flycheck-add-next-checker 'javascript-tide 'append)))))

(provide 'init-javascript.el)
;;; init-javascript.el ends here
