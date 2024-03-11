(server-start)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

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
  "Imports a init file from Emacs directory, FILE is path to init file."
  (load (locate-user-emacs-file file)))

;;;
;;; External inits
;;;
(load-init-file "init-org")
(load-init-file "init-go")
(load-init-file "init-unity")
(load-init-file "circom-mode")
(add-to-list 'auto-mode-alist '("\\.circom\\'" . circom-mode))


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
;; auto complete mini buffer completion
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
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
(global-display-line-numbers-mode)

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
  "Copies text into the system clipboard on OS X. START is first character, END is last character."
  (interactive "r")
  (shell-command-on-region start end "pbcopy"))

;; What's my email?
(set-variable 'user-mail-address "kevzettler@gmail.com")
;;(global-linum-mode 1)
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

;; Set explicit shell this is to keep shell consistent when using Tramp
(with-eval-after-load "tramp" (add-to-list 'tramp-remote-path "/root/.cargo/bin"))


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
    "Insert the current date. With PREFIX argument, use ISO format. With
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


;; ======== FLYCHECK ========
(use-package flycheck
  :ensure t
  :diminish (flycheck-mode)
  :init (global-flycheck-mode))


;;;
;;; company mode
;;;
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
  (add-to-list 'company-backends 'company-css)
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 10
   company-tooltip-flip-when-above t
   company-require-match nil
   company-begin-commands '(self-insert-command)
   )
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'complete-selection-and-expand-snippet)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  :hook (lsp-mode .company-mode)
  )

;; pos-tips show tooltip at point
(use-package pos-tip)

;;
;; Web mode
;;
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; JSX
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  ;; TSX
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (setq
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-enable-auto-quoting nil ;; disable auto quoting
   standard-indent 2
   tab-width 1
   indent-tabs-mode nil
   js-indent-level 2
   js-switch-indent-offset t
   js2-basic-offset 2
   sgml-basic-offset 2
   js2-jsx-mode 2
   js2-highlight-level 3
   js2-indent-level 2
   js2-indent-switch-body t
   js2-strict-semi-warning nil
   js2-missing-semi-one-line-override nil
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   js2-strict-trailing-comma-warning nil)
  (add-hook 'web-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook 'flycheck-mode)
  ;;(add-hook 'web-mode-hook 'eslintd-fix-mode)
  (add-hook 'web-mode-hook 'lsp-mode))




;;;
;;; LSP MODE
;;;

;; helper function from
;; https://news.ycombinator.com/item?id=37387985
;; check if webmode content is jsx
(defun gernoti/lsp-deferred-maybe-jsx ()
  (if (string= web-mode-content-type "jsx")
      (lsp-deferred)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (
         (lsp-mode . lsp-ui-mode)
         (js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . gernoti/lsp-deferred-maybe-jsx)
         )
  ;; :init ((setq lsp-keymap-prefix "C-c l")
  ;;        (setq lsp-restart 'auto-restart))
  :custom
  ;; You want this if you want to discover all lsp mode's goodies.
  ;; Though I found I actually don't use many features, really.
  (lsp-enable-which-key-integration t)
  (lsp-log-io nil) ;; Don't log everything = speed
  (lsp-eldoc-render-all t)
  (lsp-enable-snippet nil)
  (lsp-idle-delay 0.6)

  ;; Rust analyzer configs primarily for use with Ambient
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
   (lsp-ui-doc-position 'bottom)
   (lsp-ui-peek-always-show t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-sideline-show-diagnostics t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-sideline-show-code-actions t)
   (lsp-ui-doc-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))


(use-package js2-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

;;; Rust
;;; https://robert.kra.hn/posts/rust-emacs-setup/#prerequisites
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
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))



;; Prisma ORM .prisma mode
(use-package prisma-mode
  :quelpa (prisma-mode :fetcher github :repo "pimeys/emacs-prisma-mode" :branch "main"))


;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; Sass mode
(use-package sass-mode
  :mode "\\.scss\\'")

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind
  (
   ("C-x b" . ivy-switch-buffer)
   ;;("C-x C-f" . counsel-find-file)
  )
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; Swiper
;; https://github.com/abo-abo/swiper
(use-package swiper
  :ensure t
  :bind
  (
;;   ("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   )
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; Counsel
;; not suer what this does it integrates with ify and swiper and amx???
;;  https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
(use-package counsel
  :ensure t
  :diminish (counsel-mode))

;; Amx, an alternative interface for M-x in Emacs
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  :ensure t
  :bind (("M-x" . amx))
  :config
  (setq amx-backend 'ivy))

(use-package yaml-mode
  :mode ("\\.yaml" . yaml-mode))

;; Avy
;; Jump to things in Emacs tree-style
(use-package avy
  :bind ("M-s" . avy-goto-char-timer))

(use-package groovy-mode
  :mode ("Jenkinsfile?" . groovy-mode))

;; projectile
(use-package projectile
  :diminish (projectile-mode)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (counsel-projectile-mode))

;; dumb-jump
(use-package dumb-jump
  :bind (("C-c d o" . dumb-jump-go-other-window)
         ("C-c d j" . dumb-jump-go)
         ("C-c d x" . dumb-jump-go-prefer-external)
         ("C-c d z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  :init
  (dumb-jump-mode)
  :ensure
)

;; eslintd-fix
;; (use-package eslintd-fix)


;; Shaders
;; glsl files
(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)))

(use-package wgsl-mode
  :mode (("\\.wgsl\\'" . wgsl-mode)))


(defun copy-file-link-with-line-numbers ()
  "Copy a link to the current file and the line numbers of the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((file (buffer-file-name))
             (start (line-number-at-pos (region-beginning)))
             (end (line-number-at-pos (region-end)))
             (link (format "file://%s:%d-%d" file start end)))
        (kill-new link)
        (message "Copied link: %s" link))
    (message "No region selected")))




;;; init.el ends here
