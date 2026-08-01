;;; .emacs.el --- Initialization file for Emacs -*- lexical-binding: t: -*-

;;; Commentary:

;;; Code:

;; For debugging, uncomment 'profiler' in the beginning and the end of the config.
;; (require 'profiler)
;; (profiler-start 'cpu)

;; ===================================================
;; Packages
;; ===================================================
(setq custom-file (expand-file-name "~/.emacs.d/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar package-list
  '(magit
    jsonrpc
    ;; smartparens
    ;; highlight-indent-guides
    rainbow-delimiters
    vertico
    consult
    flymake-diagnostic-at-point
    flymake-ruff
    reformatter
    company
    json-mode
    markdown-mode
    markdown-preview-mode
    dockerfile-mode
    docker
    ;; docker-tramp
    rust-mode
    python-mode
    terraform-mode
    protobuf-ts-mode
    breadcrumb
    treemacs
    treemacs-magit
    ;; For claude code
    inheritenv
    ;; eat
    vterm
    dirvish
    nerd-icons
  )
)

(dolist (p package-list)
  (when (not (package-installed-p p))
    (package-install p)))

;; straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))


;; ===================================================
;; Global config
;; ===================================================
;; Key mapping
;; (keyboard-translate ?\C-h ?\C-?) does not work in 'emacs --daemon'
;; cf. https://emacsWiki.org/emacs/BackspaceKey
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<up>") 'shrink-window)
(global-set-key (kbd "C-M-<down>") 'enlarge-window)

(when (eq system-type 'darwin)
  (define-key global-map [?\M-¥] [?\\]))

;; Dired: macOS's BSD `ls` doesn't support GNU ls's `--dired` flag that Emacs
;; expects by default. Use GNU coreutils' `gls` (installed via home.nix:
;; pkgs.coreutils-prefixed) if available.
(when (eq system-type 'darwin)
  (let ((gls (executable-find "gls")))
    (if gls
        (setq insert-directory-program gls
              dired-use-ls-dired t)
      (setq dired-use-ls-dired nil))))

;; Variables
(setq-default
   wdired-allow-to-change-permissions t
   show-trailing-whitespace t
   auto-revert-check-vc-info t)

(setq
  vc-follow-symlinks t
  scroll-step 1
  inhibit-startup-screen t
  enable-local-variables t
  create-lockfiles nil
  make-backup-files nil
  delete-auto-save-files t
  display-time-day-and-date t
  show-trailing-whitespace t)

;; Auto-revert configuration (for Claude Code file changes)
(setq auto-revert-avoid-polling t)  ;; Use file notification system instead of polling
(setq auto-revert-interval 1)       ;; Check every 1 second (fallback if polling needed)
(setq auto-revert-verbose nil)      ;; Don't show messages when reverting
(global-auto-revert-mode 1)

;; Enable auto-revert only when vterm (Claude Code) is active
;; (defun enable-auto-revert-for-claude ()
;;   "Enable global-auto-revert-mode when vterm starts."
;;   (unless global-auto-revert-mode
;;     (global-auto-revert-mode 1)
;;     (message "Auto-revert enabled for Claude Code session")))

;; (defun disable-auto-revert-if-no-vterm ()
;;   "Disable global-auto-revert-mode if no vterm buffers remain."
;;   (unless (seq-some (lambda (buf)
;;                       (with-current-buffer buf
;;                         (derived-mode-p 'vterm-mode)))
;;                     (buffer-list))
;;     (global-auto-revert-mode -1)
;;     (message "Auto-revert disabled (no Claude Code sessions)")))

;; (add-hook 'vterm-mode-hook #'enable-auto-revert-for-claude)

;; Functions
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(repeat-mode 1)
(display-time-mode t)

;; Major mode
(add-hook 'emacs-lisp-mode-hook (lambda()
				  (show-paren-mode 1)
                                  (setq show-paren-delay 0))
)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-face-attribute 'fill-column-indicator nil :foreground "#555555")

;; Treesit
(setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
	(kotlin "https://github.com/fwcd/tree-sitter-kotlin")
	(proto "https://github.com/mitchellh/tree-sitter-proto")
	))

(dolist (element treesit-language-source-alist)
  (let* ((lang (car element)))
    (if (treesit-language-available-p lang)
        (message "treesit: %s is already installed" lang)
      (message "treesit: %s is not installed" lang)
      (treesit-install-language-grammar lang))))

;; ===================================================
;; Custom function
;; ===================================================
(when (eq system-type 'gnu/linux)
  (defun xsel-c()
    "Copy text on local 'kill-ring to X11's clipboard."
    (interactive)
    (when (region-active-p)
      (shell-command-on-region (region-beginning) (region-end) "xsel -ib" nil nil))))

(when (eq system-type 'darwin)
  (defun pbcopy ()
    "Copy current region to OS clipboard"
    (interactive)
    (let ((deactivate-mark t))
      (call-process-region (point) (mark) "pbcopy")))

  (defun pbpaste ()
    "Paste what has been copied"
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t)))

(defun line-on()
  "Show line number."
  (interactive)
  (display-line-numbers-mode t))

(defun line-off()
  "Hide line number."
  (interactive)
  (display-line-numbers-mode -1))

(defun hl-on()
  "Highlight current line."
  (interactive)
  (hl-line-mode t))

(defun hl-off()
  "Dehighlight current line."
  (interactive)
  (hl-line-mode -1))

;; =========================================
;; Settings by packages
;; =========================================
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
)

(use-package treemacs
  :ensure t
  :defer t
  :hook
  (treemacs-mode . hl-line-mode)
  (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq treemacs-width                 35
        treemacs-indentation           2
        treemacs-show-hidden-files     t
        treemacs-follow-after-init     t
        treemacs-no-png-images         t
        treemacs-sorting               'alphabetic-asc
        treemacs-is-never-other-window t
        treemacs-persist-file          (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when (executable-find "git")
    (treemacs-git-mode 'simple))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t C-t" . treemacs-find-file)))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package nerd-icons
  :ensure t)

(defun my/dired-open-marked-side-by-side ()
  "Open all marked files in horizontally split windows, side-by-side.
Leaves any side window (e.g. dirvish-side) untouched."
  (interactive)
  (let ((files (dired-get-marked-files))
        (main-window
         (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                   (window-list))))
    (when files
      (when main-window
        (select-window main-window))
      (delete-other-windows)
      (find-file (car files))
      (dolist (f (cdr files))
        (select-window (split-window-right))
        (find-file f))
      (balance-windows))))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes
   '(nerd-icons file-size vc-state subtree-state))
  (dirvish-side-attributes
   '(nerd-icons file-size))
  (dirvish-side-width 35) ; match treemacs-width
  (dirvish-default-layout '(0 0 0.6)) ; no parent pane, current dir + preview only
  :config
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)

  ;; dirvish-side always lands new files in the window picked by an internal
  ;; `other-window' call (cyclic order), not the window you actually focused
  ;; before switching to the sidebar. Capture the real most-recently-used
  ;; non-side window right when RET is pressed (before that internal call
  ;; runs and clobbers window-use-time), then force-select it via
  ;; `dirvish-side-open-file-action', which runs right before the file
  ;; buffer is displayed.
  (defvar my/dirvish-side-target-window nil)

  (defun my/dirvish-capture-target-window (&rest _)
    (setq my/dirvish-side-target-window
          (let ((win (get-mru-window nil nil t)))
            (and win (not (window-parameter win 'window-side)) win))))

  (advice-add 'dirvish-side-open-file :before #'my/dirvish-capture-target-window)

  (defun my/dirvish-side-restore-target-window ()
    (when (window-live-p my/dirvish-side-target-window)
      (select-window my/dirvish-side-target-window)))

  (setq dirvish-side-open-file-action #'my/dirvish-side-restore-target-window)
  :bind
  (("C-c f" . dirvish)
   ("C-x d" . dirvish-side)
   :map dirvish-mode-map
   ("TAB" . dirvish-subtree-toggle)
   ("a"   . dirvish-quick-access)
   ("s"   . dirvish-quicksort)
   ("f"   . dirvish-file-info-menu)
   ("M-F" . my/dired-open-marked-side-by-side)
   ("M-t" . dirvish-layout-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
)

;; (use-package smartparens
;;   :ensure t
;;   :hook
;;   (prog-mode text-mode markdown-mode)
;;   (after-init-hook . smartparents-global-strict-mode)
;;   :config
;;   (require 'smartparens-config)
;; )

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
)

;; (use-package highlight-indent-guides
;;   :ensure t
;;   :delight
;;   :custom
;;   (highlight-indent-guides-method  'character)
;;   (highlight-indent-guides-auto-enabled t)
;;   (highlight-indent-guides-responsive t)
;;   (highlight-indent-guides-character ?|)
;;   (set-face-background 'highlight-indent-guides-odd-face "darkgray")
;;   (set-face-background 'highlight-indent-guides-even-face "dimgray")
;;   (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
;;   :hook
;;   ((python-mode yaml-mode) . highlight-indent-guides-mode)
;; )

(use-package treesit
  :config
  (setq treesit-font-lock-level 4)
)

(use-package flymake
  :ensure t
  :bind
  (nil :map flymake-mode-map
       ("C-c C-p" . flymake-goto-prev-error)
       ("C-c C-n" . flymake-goto-next-error))
  :config
  (custom-set-faces
    '(flymake-errline
       ((((class color))
       (:foreground "Red" :bold t :underline t :background "lemon chiffon"))))
    '(flymake-warnline
       ((((class color))
     (:foreground "Red" :bold t :underline t :background "LimeGreen")))))
  ;;
  ;; (set-face-attribute 'flymake-warning nil
  ;; 		      :underline '(:style wave :color "orange"))
  ;; (set-face-attribute 'flymake-errline nil
  ;; 		      :underline '(:style wave :color "red"))
  ;; (set-face-background 'flymake-errline "gray")
  ;; (set-face-background 'flymake-warnline "darkgray")
  ;; (set-face-background 'flymake-warnline "DarkOrange")
)

;; (use-package flymake-diagnostic-at-point
;;   :ensure t
;;   :after flymake
;;   :config
;;   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;;   :hook
;;   (flymake-mode . flymake-diagnostic-at-point-mode)
;; )

(use-package company
  :ensure t
)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-stay-out-of 'company)
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
  ;; downloadSources tells the IntelliJ LSP server to fetch sources JARs for
  ;; third-party libraries (e.g. Ktor) during Gradle import, enabling M-. navigation
  ;; into library source code instead of decompiled bytecode.
  (add-to-list 'eglot-server-programs
               `(kotlin-ts-mode . ("kotlin-lsp" "--stdio"
                                   :initializationOptions
                                   (:downloadSources t))))
  (add-to-list 'eglot-server-programs
               '(protobuf-ts-mode . ("buf" "beta" "lsp")))
  :custom
  (eglot-connect-timeout 300)
  :bind
  (:map eglot-mode-map
  ("C-x C-e" . xref-find-definitions)
  ("C-x C-d" . xref-find-definitions-other-window)
  ("C-x C-r" . xref-find-references)
  ("C-x C-p" . xref-pop-marker-stack))
  :hook
  (eglot--managed-mode . company-mode)
)

;; JAR source navigation for kotlin-lsp (M-. into third-party library sources)
;;
;; Problem: When navigating to a third-party symbol (e.g. Ktor's Route), the
;; IntelliJ LSP server returns a URI like:
;;   jar:/Users/.../.gradle/.../ktor-server-core-jvm-2.3.13-sources.jar!/path/Route.kt
;;
;; Eglot doesn't recognize the "jar:" URI scheme. In Eglot 1.16+, the relevant
;; function is eglot-uri-to-path (renamed from eglot--uri-to-path). We first
;; tried advising eglot--uri-to-path, which silently did nothing because all
;; real code calls the new name. Once we found the rename via the Eglot source,
;; we fixed the advice target.
;;
;; Without the fix, Eglot treats "jar:/Users/..." as a relative path and
;; prepends the current buffer's directory, producing a garbage path like:
;;   /presentation/jar:/Users/.../.gradle/.../sources.jar!/path/Route.kt
;;
;; Fix has two parts:
;; 1. Strip the "jar:" prefix in eglot-uri-to-path so the path becomes
;;    /Users/.../.gradle/.../sources.jar!/path/Route.kt (absolute, with ! separator)
;; 2. When Emacs can't find that path on disk (it's inside a JAR archive),
;;    extract the file content via unzip and insert it into the buffer.
;;
;; We initially tried the jarchive package for this, but its internal handlers
;; were not compatible with our setup. We replaced it with these two functions.
(defun my/jar-strip-uri-prefix (orig-fun uri)
  "Strip jar: prefix from LSP URIs before eglot processes them."
  (if (string-prefix-p "jar:" uri)
      (substring uri 4)
    (funcall orig-fun uri)))

(defun my/jar-find-file-not-found ()
  "Extract and display a file from inside a JAR (*.jar!/inner/path)."
  (let ((file buffer-file-name))
    (when (and file (string-match "\\(.*\\.jar\\)!\\(/.*\\)" file))
      (let* ((jar-file (match-string 1 file))
             (inner-path (string-trim-left (match-string 2 file) "/"))
             (content (shell-command-to-string
                       (format "unzip -p %s %s 2>/dev/null"
                               (shell-quote-argument jar-file)
                               (shell-quote-argument inner-path)))))
        (when (> (length content) 0)
          (erase-buffer)
          (insert content)
          (set-buffer-modified-p nil)
          t)))))

(with-eval-after-load 'eglot
  (advice-add 'eglot-uri-to-path :around #'my/jar-strip-uri-prefix)
  (add-to-list 'find-file-not-found-functions #'my/jar-find-file-not-found))

(use-package breadcrumb
  :ensure t
  :hook
  (eglot--managed-mode . breadcrumb-local-mode)
)

;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :ensure t
;;   :hook
;;   (prog-mode . copilot-mode)
;;   :bind
;;   (:map copilot-completion-map
;; 	("<tab>" . 'copilot-accept-completion)
;;         ("TAB" . 'copilot-accept-completion)
;;         ("C-TAB" . 'copilot-accept-completion-by-word)
;;         ("C-<tab>" . 'copilot-accept-completion-by-word))
;; )

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  ;; (add-to-list 'eglot-server-programs
  ;;              '((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  :hook
  (rust-ts-mode . eglot-ensure)
)

;; Go
(use-package go-ts-mode
  :ensure t
  :mode
  ("\\.go\\'" . go-ts-mode)
  ("\\.go\\.mod\\'" . go-mod-ts-mode)
  :init
  (setq go-mode-treesitter-derive t)

  ;; Define a proper project type for Go modules
  (require 'project)
  (require 'cl-lib)

  ;; Project type
  (cl-defstruct (go-module-project
		 (:constructor go-module-project-create (&key root)))
  root)

  (defun project-find-go-module (dir)
    "Detect a Go module project rooted at go.mod above DIR."
    (if-let ((root (locate-dominating-file dir "go.mod")))
	(go-module-project-create
	 :root (file-name-as-directory root))))

  (cl-defmethod project-root ((project go-module-project))
    (go-module-project-root project))
  (add-hook 'project-find-functions #'project-find-go-module 0)

  ;; Formatting helpers
  (defun eglot-format-buffer-on-save ()
    (when (and (boundp 'eglot--managed-mode) eglot--managed-mode)
      (ignore-errors (eglot-format-buffer))))

  (defun go-format-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer-on-save -10 t))

  :hook
  (go-ts-mode . eglot-ensure)
  (go-ts-mode . go-format-on-save)
)

;; Python
(use-package flymake-ruff
  :ensure t
  :after flymake
  :custom
  (flymake-ruff-program '("uv" "run" "ruff"))
  :hook
  (python-ts-mode . flymake-ruff-load)
)

(use-package reformatter
  :ensure t
  :hook
  (python-ts-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-sort-imports-on-save-mode)
  :config
  (reformatter-define yamlfmt
    :program "yamlfmt"
    :args '("-in"))
  (reformatter-define ruff-format
    :program "uv"
    :args `("run" "ruff" "format" "--stdin-filename", buffer-file-name "-"))
  (reformatter-define ruff-sort-imports
    :program "uv"
    :args `("run" "ruff" "check" "--select" "I" "--fix" "--stdin-filename", buffer-file-name "-"))
)

(defun ktlint-format-buffer ()
  "Format current buffer with ktlint via a temp file."
  (interactive)
  (let* ((ext (if (string= (file-name-extension buffer-file-name) "kts") ".kts" ".kt"))
         (temp-file (make-temp-file "ktlint-" nil ext))
         (current-point (point)))
    (write-region (point-min) (point-max) temp-file nil)
    (shell-command-to-string (format "ktlint --format %s" temp-file))
    (erase-buffer)
    (insert-file-contents temp-file)
    (delete-file temp-file)
    (goto-char current-point)))

(defun ktlint-format-before-save ()
  (when (memq major-mode '(kotlin-ts-mode))
    (ktlint-format-buffer)))

(defun ruff-fix-buffer ()
  "Use ruff to fix lint violations in the current buffer."
  (interactive)
  (let* ((temporary-file-directory (if (buffer-file-name)
                                       (file-name-directory (buffer-file-name))
                                     temporary-file-directory))
         (temporary-file-name-suffix (format "--%s" (if (buffer-file-name)
                                                                 (file-name-nondirectory (buffer-file-name))
                                                                "")))
         (temp-file (make-temp-file "temp-ruff-" nil temporary-file-name-suffix))
         (current-point (point)))
    (write-region (point-min) (point-max) temp-file nil)
    (shell-command-to-string (format "uv run ruff check --fix %s" temp-file))
    (erase-buffer)
    (insert-file-contents temp-file)
    (delete-file temp-file)
    (goto-char current-point))
)

(defun ruff-fix-before-save ()
  (interactive)
  (when (memq major-mode '(python-mode python-ts-mode))
    (ruff-fix-buffer)))

(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-ts-mode)
  :init
  (setq python-mode-treesitter-derive t)
  :hook
  (python-ts-mode . display-fill-column-indicator-mode)
  (python-ts-mode . (lambda()
		      (setq fill-column 79)))
  (python-ts-mode . eglot-ensure)
  (before-save . ruff-fix-before-save)
)

(use-package typescript-ts-mode
  :ensure t
  :mode
  ("\\.ts\\'" . tsx-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.mts\\'" . tsx-ts-mode)
  ("\\.js\\'" . tsx-ts-mode)
  ("\\.jsx\\'" . tsx-ts-mode)
  ("\\.mjs\\'" . tsx-ts-mode)
  ("\\.cjs\\'" . tsx-ts-mode)
  :config
  (setq typescript-ts-mode-indent-offset 2)
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
)

;; Kotlin
(cl-defstruct (gradle-project
               (:constructor gradle-project-create (&key root)))
  root)

(defun project-find-gradle-root (dir)
  "Detect a Gradle project rooted at settings.gradle.kts above DIR."
  (if-let ((root (locate-dominating-file dir "settings.gradle.kts")))
      (gradle-project-create :root (file-name-as-directory root))))

(cl-defmethod project-root ((project gradle-project))
  (gradle-project-root project))

(add-hook 'project-find-functions #'project-find-gradle-root 0)

(use-package kotlin-ts-mode
  :vc (:url "https://gitlab.com/bricka/emacs-kotlin-ts-mode" :rev :newest)
  :mode
  ("\\.kt\\'" . kotlin-ts-mode)
  ("\\.kts\\'" . kotlin-ts-mode)
  :hook
  (kotlin-ts-mode . eglot-ensure)
  (before-save . ktlint-format-before-save)
)

(use-package protobuf-ts-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-ts-mode)
  :config
  (setq-local c-basic-offset 2)
  :hook
  (protobuf-ts-mode . eglot-ensure)
)

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode)
)

(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker)
)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t)
  (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook #'turn-on-visual-line-mode))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :commands (markdown-preview markdown-preview-mode)
  :bind (:map markdown-mode-map
              ("C-c C-c v" . markdown-preview)
              ("C-c C-c l" . markdown-preview-mode))
  :config
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.1/github-markdown.min.css")))

(with-eval-after-load 'markdown-mode
  (dolist (m '(("python" . python-ts-mode)
               ("rust"   . rust-ts-mode)
               ("go"     . go-ts-mode)
               ("ts"     . tsx-ts-mode)
               ("tsx"    . tsx-ts-mode)
               ("json"   . json-mode)
               ("yaml"   . yaml-ts-mode)
               ("kotlin" . kotlin-ts-mode)))
    (add-to-list 'markdown-code-lang-modes m)))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :hook
  (json-mode . (lambda ()
		 (setq js-indent-level 2)
		 (setq-default indent-tabs-mode nil))))

(use-package yaml-ts-mode
  :ensure t
  :mode
  ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook
  (yaml-ts-mode . eglot-ensure)
  (yaml-ts-mode . yamlfmt-on-save-mode)
)

(use-package terraform-mode
  :ensure t
  :custom
  (terraform-indent-level 2)
  :hook
  (terraform-mode . eglot-ensure)
  (terraform-mode . outline-minor-mode)
  (terraform-mode . terraform-format-on-save-mode)
)

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for eat terminal backend:
;; (use-package eat :ensure t)

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest)
)

;; for vterm terminal backend:
(use-package vterm
  :ensure t)

(defun display-claude-on-right (buffer)
  (display-buffer buffer '((display-buffer-in-side-window)
			   (side . right)
			   (window-width . 90))))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :init
  (setq claude-code-terminal-backend 'vterm)
  ;; Optionally define a repeat map so that "M" will cycle thru Claude
  ;; auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  ;; :bind
  ;; (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

  :config
  (setq claude-code-display-window-fn #'display-claude-on-right)

  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :hook
  (vterm-mode . (lambda ()
		  (setq-local show-trailing-whitespace nil)
		  (display-line-numbers-mode -1)
		  ;; Disable auto-revert when this vterm buffer is killed
		  (add-hook 'kill-buffer-hook #'disable-auto-revert-if-no-vterm nil t)))
  )

;;; .emacs.el ends here
