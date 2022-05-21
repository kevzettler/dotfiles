(server-start)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

;; Save adding :ensure t on every use package
(setq use-package-always-ensure t)

;; Preserve highlights in panes when switching
(setq highlight-nonselected-windows t)

;; Ensure environment variables inside Emacs look the same as in the user's shell.
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
   This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(use-package exec-path-from-shell
	     :ensure t
	     :config
	     (when (memq window-system '(mac ns))
	       (exec-path-from-shell-initialize)
	       (set-exec-path-from-shell-PATH))
	     ;; ensure shell variables match environment
	     ;; https://github.com/purcell/exec-path-from-shell
	     ;; used for running eshell from Mac Osx Emacs
	     (when (memq window-system '(mac ns x))
	       (exec-path-from-shell-initialize))	     
	     )



;;
;; Configure and load secrets
;;
(setf epa-pinentry-mode 'loopback)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
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
(load-init-file "init-org")
(load-init-file "init-company") ;; tern is in here too
(load-init-file "init-javascript")
(load-init-file "init-go")
;;(load-init-file "init-reason")
(load-init-file "init-unity")

(use-package php-mode :ensure t)

;; make sure the display is clean to start with
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")

;;
;; Theme
;;
(use-package solarized-theme
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/color-theme-solarized-20140408.1309")
  (load-theme `solarized-light t))

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
;; Configure backup file creation in it's own directory
(defvar bry/backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p bry/backup-directory)
  (make-directory bry/backup-directory t))
(setq backup-directory-alist `(("." . ,bry/backup-directory))
      make-backup-files t
      version-control t
      backup-by-copying-when-linked t
      delete-old-versions t
      delete-by-moving-to-trash t)

(setq create-lockfiles nil)
;(setq make-backup-files t)
;;(setq version-control t)
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
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
      (append '(("\.kiwi$"  . idl-mode))
              auto-mode-alist))

(setq auto-mode-alist
      (append '(("\.yaml$"  . yaml-mode))
              auto-mode-alist))

(setq auto-mode-alist
      (append '(("\.mini$"  . rust-mode))
                auto-mode-alist))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; multiple-cursors-mode
(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("M-d" . mc/mark-all-like-this)
         ("s-<mouse-1>" . mc/add-cursor-on-click)))

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

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; NO alt meta
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\m" 'execute-extended-command)
(global-set-key "\C-c\m"   'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)



(global-set-key (kbd "C-;") 'goto-line)

;; Expand to the region
(use-package expand-region
  :bind ("C-'" . er/expand-region))

(use-package terraform-mode
  :mode (("\\.tf" . terraform-mode))
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;;
;;; Shell
;;
(defun my-eshell-remove-pcomplete ()
  (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t))

(add-hook 'eshell-mode-hook #'my-eshell-remove-pcomplete)


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
(put 'narrow-to-region 'disabled nil)


;;; Rust stuff
;;;
;;;
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))



;;;
;;; LSP MODE
;;;
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


(with-eval-after-load "lsp-rust"
 (lsp-register-client
  (make-lsp-client
   :new-connection (lsp-stdio-connection
                    (lambda ()
                      `(,(or (executable-find
                              (cl-first lsp-rust-analyzer-server-command))
                             (lsp-package-path 'rust-analyzer)
                             "rust-analyzer")
                        ,@(cl-rest lsp-rust-analyzer-server-args))))
   :remote? t
   :major-modes '(rust-mode rustic-mode)
   :initialization-options 'lsp-rust-analyzer--make-init-options
   :notification-handlers (ht<-alist lsp-rust-notification-handlers)
   :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
   :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
   :after-open-fn (lambda ()
                    (when lsp-rust-analyzer-server-display-inlay-hints
                      (lsp-rust-analyzer-inlay-hints-mode)))
   :ignore-messages nil
   :server-id 'rust-analyzer-remote)))

;; Set explicit shell this is to keep shell consistent when using Tramp
(with-eval-after-load "tramp" (add-to-list 'tramp-remote-path "/root/.cargo/bin"))

;;; init.el ends here
