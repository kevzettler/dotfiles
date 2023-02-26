;; Helper fn that selects the company auto-complete
;; then we just try to expand it.
(defun complete-selection-and-expand-snippet ()
  (interactive)
  (company-complete-selection)
;;  (yas/expand)
  )

;; Autocomplete Popups
(use-package company
  :defer t
  :diminish company-mode
  :init (global-company-mode 1)
  :config
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backends.")

  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;       backend
  ;;     (append (if (consp backend) backend (list backend))
  ;;             '(:with company-yasnippet))))
  (add-to-list 'company-backends 'company-css)
;;  (add-to-list 'company-backends 'company-yasnippet)
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 10
   company-tooltip-flip-when-above t
;;   company-dabbrev-downcase nil
   company-require-match nil
   company-begin-commands '(self-insert-command)
;;   company-backends (mapcar #'company-mode/backend-with-yas company-backends)
   )
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'complete-selection-and-expand-snippet)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; Company-web
(use-package company-flow
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-flow))
  :config
  (add-to-list 'company-flow-modes 'rjsx-mode))


(use-package company-web
  :init (add-to-list 'company-backends 'company-web-html))


;; pos-tips show tooltip at point
(use-package pos-tip)

(provide 'init-company)
;;; init-company ends here
