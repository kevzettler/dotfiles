;; Org mode
(message "loading org init")
(require 'time-stamp)


;; org-mode image drag and drop
(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


;; package for controling silver surfer
;; enables projectile-ag
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

;;(require 'org-install)
;;(message "require org-install")
(setq org-directory "~/Dropbox (Personal)/org")
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


;;
;; Custom journal commands (combined journal.org; no org-journal package)
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

(defun journal--combined-file ()
  "Path to the single journal file (top-level headings = YYYY-MM-DD)."
  (expand-file-name "journal.org" org-directory))

(defun journal--daily-file-name-p (path)
  "Non-nil if PATH is a per-day ISO journal file at org root (YYYY-MM-DD.org)."
  (string-match-p "/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\.org\\'" path))

(defconst journal--iso-date-level-1-re
  "^\\* \\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)\\(?:[ \t]\\|$\\)"
  "First-level Org headline whose title is an ISO date (YYYY-MM-DD).")

(defun journal--chronological-insert-point (date-string)
  "Buffer position where a new `* DATE-STRING' tree belongs.
Keeps all such ISO date subtrees in ascending chronological order.
DATE-STRING must be YYYY-MM-DD (lexicographic order matches time order)."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (catch 'journal-insert-pos
      (let (after-last)
        (while (re-search-forward journal--iso-date-level-1-re nil t)
          (let ((here-date (match-string 1))
                (here-beg (match-beginning 0)))
            (if (string-lessp date-string here-date)
                (throw 'journal-insert-pos here-beg)
              (goto-char here-beg)
              (org-end-of-subtree nil t)
              (setq after-last (point)))))
        (throw 'journal-insert-pos
               (or after-last (progn (goto-char (point-max)) (point))))))))

(defun journal--insert-new-date-tree (date-string heading)
  "Insert HEADING (a `* YYYY-MM-DD' line) plus the usual template at the right place."
  (let ((pos (journal--chronological-insert-point date-string)))
    (goto-char pos)
    (unless (bolp) (insert "\n"))
    (insert heading "\n\n"
            "** Positives\n\n\n"
            "** Intentions for tomorrow\n\n\n"
            "** Checkin\n\n\n"
            "** Learnings\n\n\n"
            "** Time sucks\n\n")
    (goto-char pos)
    (org-back-to-heading t)
    (org-show-context 'agenda)))

(defun journal-create-or-open-daily-file (&optional days-offset)
  "Open today's section inside `journal.org' under heading * YYYY-MM-DD.
Creates that subtree with the usual template if missing.  New trees are
inserted so ISO date headings stay in chronological order (so yesterday /
tomorrow work next to today).  Per-day YYYY-MM-DD.org files on disk are
left unchanged (use the merge script for history).

If DAYS-OFFSET is non-nil, use that many days from today (negative = past)."
  (interactive)
  (let* ((adjusted-date (time-add (current-time) (days-to-time (or days-offset 0))))
         (date-string (format-time-string "%Y-%m-%d" adjusted-date))
         (jpath (journal--combined-file))
         (heading (concat "* " date-string))
         (heading-re (concat "^\\* " (regexp-quote date-string) "\\($\\|[ \t]\\)")))
    (find-file jpath)
    (unless (file-exists-p jpath)
      (insert "#+TITLE: Journal\n\n"))
    (widen)
    (goto-char (point-min))
    (if (re-search-forward heading-re nil t)
        (progn
          (org-back-to-heading t)
          (org-show-context 'agenda))
      (journal--insert-new-date-tree date-string heading))))

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
;; Org binds C-j to newline-and-indent on `org-mode-map', shadowing the global binding.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-j") 'journal-create-or-open-daily-file))
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


;; Omit per-day YYYY-MM-DD.org at org root so agenda does not scan thousands of files.
;; Combined journal lives in journal.org; see merge_daily_journals_to_journal_org.py
(defun journal--rebuild-org-agenda-files ()
  "Set `org-agenda-files' to org files under `org-directory', excluding ISO-dated dailies."
  (setq org-agenda-files
        (cl-remove-if #'journal--daily-file-name-p
                      (directory-files org-directory 'full "^[^#].*\\.org\\'" t))))

(add-hook 'org-agenda-prepare-hook #'journal--rebuild-org-agenda-files)
(journal--rebuild-org-agenda-files)



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
