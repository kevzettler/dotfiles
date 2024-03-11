;; Org mode
(message "loading org init")
(require 'time-stamp)

;; package for controling silver surfer
;; enables projectile-ag
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

;;(require 'org-install)
;;(message "require org-install")
(setq org-directory "~/Dropbox (Personal)/org")
(setq org-journal-directory (concat org-directory "/journal"))
(message "org directory")

(require 'org-tempo)

;; (load-file "~/.emacs.d/plugins/emacs-grammarly/emacs-grammarly.el")
;; (global-set-key (kbd "C-c C-g") 'grammarly-save-region-and-run)

(setq org-archive-location "~/Dropbox (Personal)/org/archive/%s_archive::")
(setq org-log-done 'time)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq auto-mode-alist
      (append '(("\.todo$"  . org-mode))
              auto-mode-alist))

(setq auto-mode-alist
      (append '(("\.org\.txt$"  . org-mode))
              auto-mode-alist))

(setq org-export-backends '(ascii html latex md))


(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Configure Hunspell
(setq ispell-program-name "hunspell")
(setq ispell-really-hunspell t)
(setq ispell-dictionary "en_US") ; Replace "en_US" with your desired dictionary

;; Set paths for Hunspell dictionaries
(setq ispell-local-dictionary-alist
      '((nil ; default dictionary
         "[[:alpha:]]" "[^[:alpha:]]" "[']" t
         ("-d" "en_US") ; Dictionary file name, replace "en_US" if needed
         nil utf-8)))
(setq ispell-dictionary "en_US") ; Set the default dictionary
(setq ispell-hunspell-dict-paths-alist
      '(("en_US" "/Library/Spelling/en_US.aff" "/Library/Spelling/en_US.dic")))


(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(defun my/flyspell-hack ()
  (flyspell-mode 1)
  (flyspell-mode -1))

(add-hook 'org-mode-hook 'my/org-mode-hook)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'my/flyspell-hack)

;;NO spell check for embedded snippets
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let ((rlt ad-return-value)
        (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
        (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
        old-flag
        b e)
    (when ad-return-value
      (save-excursion
        (setq old-flag case-fold-search)
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))


(setq org-todo-keywords
           '((sequence "TODO" "|" "DONE" "WONTDO")
             (sequence "UNREAD" "READ")
             (sequence "???" "OK")))

;; common lisp emulation for babel? not sure
(require 'cl-lib)

;;
;; org-babel
;;

(setenv "NODE_PATH"
  (concat
   (getenv "HOME") "/org/node_modules"  ":"
   (getenv "NODE_PATH")
  )
)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)))


(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/Dropbox (Personal)/org/journal"
        org-journal-date-format "%A, %d %B %Y"))
;;
;; Custom journal commands
;;
(defun kill-buffer-ask (buffer)
  "Kill BUFFER if confirmed and modified, or kill without confirmation if unmodified."
  (if (buffer-modified-p buffer)
      (when (yes-or-no-p (format "Buffer %s HAS BEEN EDITED. Kill? " (buffer-name buffer)))
        (kill-buffer buffer))
    (kill-buffer buffer)))

(defun bk-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
        (kill-matching-buffers regexp)))

(defun kill-journals ()
  "Kill open org journal buffers."
  (interactive)
  (bk-kill-buffers "\.org$"))


(defun journal-create-or-open-daily-file (&optional days-offset)
  "Create a daily journal org file with the current date as the prefix.
if DAYS-OFFSET is provided, the date will be adjusted accordingly."
  (interactive)
  (let* ((adjusted-date (time-add (current-time) (days-to-time (or days-offset 0))))
         (date-string (format-time-string "%Y-%m-%d" adjusted-date))
         (file-name (concat date-string ".org"))
         (file-path (expand-file-name file-name org-directory))
         (initial-content-template
          (concat "#+TITLE: " date-string "\n\n* Positives\n\n\n* Intentions for tomorrow\n\n\n* Checkin\n\n\n* Learnings\n\n\n* Time sucks\n\n")))
    (unless (file-exists-p file-path)
      (with-temp-buffer
        (insert initial-content-template)
        (write-file file-path)))
    (find-file file-path)))

(defun yesterday ()
  "Open up the journal for yesterday."
  (interactive)
  (journal-create-or-open-daily-file -1))

(defun tomorrow ()
  "Open up the journal for tomorrow."
  (interactive)
  (journal-create-or-open-daily-file 1))

(global-set-key "\C-j" 'journal-create-or-open-daily-file)
(global-set-key "\C-xj" 'journal-create-or-open-daily-file)
(global-set-key "\C-cj" 'journal-create-or-open-daily-file)
(journal-create-or-open-daily-file)


(defun capture-comment-line (&optional line)
  (let ((c
        (save-excursion
          (save-window-excursion
            (switch-to-buffer (plist-get org-capture-plist :original-buffer))
          comment-start)
          )))
    (while (string-prefix-p c line)
      (setq line (string-remove-prefix c line)))
    (comment-string-strip line t t)
    ))

(setq org-capture-templates
      '(("C" "TODO code comment" entry (file+headline "~/org/brain/code.org" "Tasks")
        "* %(capture-comment-line \"%i\")\n  %a"
        )))


(setq org-agenda-files (list org-directory
                             org-journal-directory
                             ))



(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;
;; GCAL INTEGRATION
;;
(setq package-check-signature nil)

(use-package org-gcal
  :ensure t
  :init
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
  :config
  (require 'secrets)
  (setq org-gcal-file-alist '(("kevzettler@gmail.com" . "~/Dropbox (Personal)/org/gcal.org"))))
  ;; (defun new/org-gcal--notify (title mes)
  ;;   (message "org-gcal::%s - %s" title mes))
  ;; (fset 'org-gcal--notify 'new/org-gcal-notify))


(use-package org-download
  :ensure t
  :defer t
  :config
  (setq-default org-download-image-dir "~/Dropbox/org/images"))
