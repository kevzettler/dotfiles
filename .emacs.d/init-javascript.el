;; Counsel
(use-package counsel
    :diminish (counsel-mode))

;; Ivy
(use-package ivy
  :diminish (ivy-mode)
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; Swiper
(use-package swiper
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; Avy
(use-package avy
  :bind ("M-s" . avy-goto-char-timer))

;; projectile
(use-package projectile
  :diminish (projectile-mode)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :config
  (counsel-projectile-on))

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





;; Web mode
(use-package web-mode
  :mode (("\\.js[x]?\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.yaml\\'" . web-mode)
         ("\\.todo\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.py\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (hs-minor-mode t)))
  (setq
   web-mode-enable-auto-closing nil
   web-mode-enable-auto-pairing nil
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-enable-css-colorization t
   web-mode-markup-indent-offset 2
   web-mode-enable-current-column-highlight t
   ;; set web-mode-content-type to jsx for js and jsx files
   web-mode-content-types-alist
   '(("jsx" . "\\.js[x]?\\'")))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))


;; Autocomplete Popups
(use-package company
  :diminish company-mode
  :config
  (global-company-mode 1)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (add-to-list 'company-backends 'company-css)
  (add-to-list 'company-backends 'company-yasnippet)
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 10
   company-tooltip-flip-when-above t
   company-dabbrev-downcase nil
   company-require-match nil
   company-begin-commands '(self-insert-command)
   company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; Company-web
(use-package company-web
  :init (add-to-list 'company-backends 'company-web-html))

;; tern (js)
(use-package tern
  :diminish tern-mode
  :config
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'web-mode-hook 'tern-mode)
  (setq tern-command (append tern-command '("--no-port-file"))))

;; Company integration for tern (js)
(use-package company-tern
  :init (add-to-list 'company-backends '(company-tern company-web-html :with company-yasnippet))
  :config (setq company-tern-property-marker nil))

;; pos-tips show tooltip at point
(use-package pos-tip)


;; flycheck
(use-package flycheck-flow)
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (setq-default flycheck-disabled-checkers '(javascript-standard))
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint 'typescript-tslint)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (defun my/use-flow-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (flow (and root
                      (expand-file-name "node_modules/flow-bin/vendor/flow"
                                        root))))
      (when (and flow (file-executable-p flow))
        (setq-local flycheck-javascript-flow-executable flow))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules))


;; multiple-cursors-mode
(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("M-d" . mc/mark-all-like-this)
         ("s-<mouse-1>" . mc/add-cursor-on-click)))
