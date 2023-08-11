;; circom-mode.el -- Circom Languaje Major Mode

;;; Commentary:
;; 2020 - @hugo-dc
;;
;; This file is not part of GNU Emacs

;;; Code:
(setq circom-keywords '("template" "signal" "private" "input" "output" "component" "include" "var" "for" "if" "else") )

;; create regex string for each category
(setq circom-keywords-regexp ( regexp-opt circom-keywords 'words))

;; create the list for font lock
(setq circom-font-lock-keywords
      `(
        (,circom-keywords-regexp . font-lock-keyword-face)
        ))

;;;###autoload
(define-derived-mode circom-mode c-mode
  "Circom Language Mode"
  "Major mode for the Circom Language"
  ;;(modify-syntax-entry ?\/ ". 12b")
  ;;(modify-syntax-entry ?\n "> b")
  ;;(modify-syntax-entry ?\/ ". 14")
  ;;(modify-syntax-entry ?* ". 23")
  ;;(setq-local comment-start "/*")
  ;;(setq-local comment-end "*/")
  ;; Code for syntax highlighting
  (setq font-lock-defaults '((circom-font-lock-keywords))))

(setq circom-keywords nil)
(setq circom-keywords-regexp nil)

(provide 'circom-mode)

;;; circom-mode ends here

