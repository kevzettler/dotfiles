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
