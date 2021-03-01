;; Org mode
(message "loading org init")

;; package for controling silver surfer
;; enables projectile-ag
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(require 'org-install)
(message "require org-install")
(setq org-directory "~/Dropbox (Personal)/org")
(message "org directory")

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


(use-package org-journal
  :config
  (setq org-journal-dir "~/Dropbox (Personal)/org/test-journal"))


(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
(add-hook 'org-mode-hook 'turn-on-flyspell)

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


;;
;; Custom journal commands
;;
(defun bk-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
        (kill-matching-buffers regexp)))

(defun kill-journals ()
  "Kill open org journal buffers"
  (interactive)
  (bk-kill-buffers "\.org$"))

(defun journal ()
  "Create or opens a daily journal entry."
  (interactive)
  (let* ((daily-name (format-time-string "%Y-%m-%d"))
         (dropbox-path "~/Dropbox (Personal)/")
         (code-path (concat dropbox-path "code/org-stream"))
         (journal-path (concat dropbox-path "org/journal/"))
         (journal-file (concat journal-path daily-name ".org")))
     (unless (file-exists-p journal-file)
       (shell-command "cd ~/code/personal-projs/org-stream && node createDaily.js"))
     (find-file journal-file)))

(defun today ()
  (journal))

(global-set-key "\C-j" 'journal)
(global-set-key "\C-xj" 'journal)
(global-set-key "\C-cj" 'journal)
(journal)


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


(setq org-agenda-files (list "~/Dropbox (Personal)/org"
                             "~/Dropbox (Personal)/org/journal"
                             "~/Google Drive/Landord Party Closet"
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


;;
;; Org to blog
;;
(use-package org2blog
  :ensure t
  :defer t
  :config
  (setq org2blog/wp-blog-alist
      '(("radcade"
         :url "https://radcade.com/xmlrpc.php"
         :username "kevzettler")))
  )
