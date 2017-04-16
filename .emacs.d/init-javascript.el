;; Web mode
(use-package web-mode
  :mode (("\\.js[x]?\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.yaml\\'" . web-mode)
         ("\\.todo\\'" . web-mode)
         ("\\.py\\'" . web-mode))
  :config
  (setq
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
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (setq-default flycheck-disabled-checkers '(javascript-standard))
  (with-eval-after-load 'flycheck
    ;; (flycheck-add-mode 'javascript-standard 'js2-mode)
    ;; (flycheck-add-mode 'javascript-standard 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; (flycheck-add-mode 'html-tidy 'web-mode)
    ;; (flycheck-add-mode 'css-csslint 'web-mode))
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))


;; multiple-cursors-mode
(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("M-d" . mc/mark-all-like-this)
         ("s-<mouse-1>" . mc/add-cursor-on-click)))
