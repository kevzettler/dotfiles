(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

;; Save adding :ensure t on every use package
(setq use-package-always-ensure t)

;; Ensure environment variables inside Emacs look the same as in the user's shell.
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;
;; Configure and load secrets
;;
(setf epa-pinentry-mode 'loopback)
(custom-set-variables '(epg-gpg-program  "/usr/local/opt/gnupg\@2.1/bin/gpg2"))
(load (expand-file-name "secrets.el" user-emacs-directory))


;;;
;;; Load Path
;;;
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(defun load-init-file (file)
  (load (locate-user-emacs-file file)))

;;;
;;; External inits
;;;
(load-init-file "init-javascript")
(load-init-file "init-org")
(load-init-file "init-reason")


;; make sure the display is clean to start with
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")

;;
;; Theme
;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/color-theme-solarized-20140408.1309")
(load-theme `solarized-light t)
;;(load-theme `solarized-dark t)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)


;; Bar cursor instead of 70s highlight thing
(setq-default cursor-type 'bar)


;;; Automatically reload files that were changed on disk, if they have
;;; not been modified in Emacs since the last time they were saved.
(setq auto-revert-verbose nil)

(global-auto-revert-mode 1)

;;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
(setq auto-revert-interval 1)
;; IDO mode
(use-package ido
  :config
  (ido-mode 'both)
  (setq
   ido-mode 1
   ido-everywhere t
   ido-enable-flex-matching t
   ido-max-directory-size 100000
   ;; Use the current window when visiting files and buffers with ido
   ido-default-file-method 'selected-window
   ido-default-buffer-method 'selected-window))
(ido-mode 1)
(ido-everywhere 1)

;; taken from http://functionalrants.wordpress.com/2008/09/06/how-to-set-up-emacs-slime-sbcl-under-gnulinux/
;; Text and the such
;; Use colors to highlight commands, etc.
(global-font-lock-mode t)

;; Disable the welcome message
(setq inhibit-startup-message t)

;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)

;; Flash instead of that annoying bell
(setq visible-bell t)

;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)

; lines
(setq scroll-step 1)
(line-number-mode 1)
(column-number-mode 1)
 
; backup
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq auto-save-file-name-transforms`((".*" ,"~/.emacs_backups/" t)))
; ----------------------------------------

;scrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq auto-mode-alist
      (append '(("\.scss$"  . sass-mode))
              auto-mode-alist))

(setq auto-mode-alist
      (append '(("\.yaml$"  . yaml-mode))
                auto-mode-alist))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

; paren hilite
; http://www.emacsblog.org/2007/08/07/quick-tip-show-paren-mode/
(show-paren-mode t)
; (setq show-paren-style 'expression)

; http://infolab.stanford.edu/~manku/dotemacs.html
(transient-mark-mode t) ; highlight marked region

(setq-default delete-old-versions 't)

(setq-default indent-tabs-mode nil)

;; warp around search
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))
; ----------------------------------------
 
(setq resize-minibuffer-mode t)
 
(defun pbcopy-region (start end)
  "Copies text into the system clipboard on OS X"
  (interactive "r")
  (shell-command-on-region start end "pbcopy"))

;; What's my email?
(set-variable 'user-mail-address "kevzettler@gmail.com")
(global-linum-mode 1)
(setq-default truncate-lines "f")


;; Helpful key bindings
(global-set-key (kbd "C-x ;") 'goto-line)
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "s-/") 'comment-dwim)
(global-set-key (kbd "s-[") 'hs-hide-block)
(global-set-key (kbd "s-]") 'hs-show-block)

;; NO alt meta
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\m"   'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "C-;") 'goto-line)

;; Expand to the region
(use-package expand-region
  :bind ("C-'" . er/expand-region))


;; Shell key bindings
(progn(require 'comint)
      (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
      (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
      (define-key comint-mode-map (kbd "s-k") 'erase-buffer))


(add-hook 'eshell-mode-hook '(lambda ()
                               (local-set-key (kbd "s-k") 'erase-buffer)))

(put 'erase-buffer 'disabled nil)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(defun do-nvm-use (version)
  (interactive "sVersion: ")
  (nvm-use version)
  (exec-path-from-shell-copy-env "PATH"))

;;; Fix junk characters in shell-mode
(add-hook 'shell-mode-hook 
          'ansi-color-for-comint-mode-on)
(setenv "NODE_NO_READLINE" "1")
(add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))


(setq org-agenda-files (list "~/Dropbox (Personal)/org"
                             "~/Dropbox (Personal)/org/journal"
                             "~/Dropbox (Personal)/org/brain/todo.org"                                                          
                             "~/Dropbox (Personal)/org/gcal.org"))



;; GCAL INTEGRATION
(setq package-check-signature nil)

(use-package org-gcal
  :ensure t
  :init
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
  :config
  (require 'secrets)
  (setq org-gcal-file-alist '(("kevzettler@gmail.com" .  "~/org/gcal.org")))
  (defun new/org-gcal--notify (title mes)
    (message "org-gcal::%s - %s" title mes))
  (fset 'org-gcal--notify 'new/org-gcal-notify))



(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          (system-time-locale "de_DE"))
      (insert (format-time-string format))))



;;;
;;; Customize
;;;
(setq custom-file (locate-user-emacs-file "init-custom.el"))
(load custom-file)
