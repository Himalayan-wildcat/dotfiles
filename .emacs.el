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
    smartparens
    rainbow-delimiters
    highlight-indent-guides
    vertico
    consult
    flymake-diagnostic-at-point
    flymake-ruff
    reformatter
    company
    yaml-mode
    dockerfile-mode
    docker
    docker-tramp
    rust-mode
    python-mode)
)

(dolist (p package-list)
  (when (not (package-installed-p p))
    (package-install p)))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


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

;; Functions
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(display-time-mode t)

;; Major mode
(add-hook 'emacs-lisp-mode-hook (lambda()
				  (show-paren-mode 1)
                                  (setq show-paren-delay 0))
)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
)

(use-package smartparens
  :ensure t
  :hook
  (prog-mode text-mode markdown-mode)
  (after-init-hook . smartparents-global-strict-mode)
  :config
  (require 'smartparens-config)
)

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

(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode)
)

(use-package company
  :ensure t
)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-stay-out-of 'company)
  :bind
  (:map eglot-mode-map
  ("C-x C-e" . xref-find-definitions)
  ("C-x C-d" . xref-find-definitions-other-window)
  ("C-x C-r" . xref-find-references)
  ("C-x C-p" . xref-pop-marker-stack))
  :hook
  (eglot--managed-mode . company-mode)
)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
	("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word))
)

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode . eglot-ensure)
)

;; Python
(use-package flymake-ruff
  :ensure t
  :after flymake
  :hook
  (python-mode . flymake-ruff-load)
)

(use-package reformatter
  :ensure t
  :hook
  (python-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename", buffer-file-name "-"))
  (reformatter-define ruff-sort-imports
    :program "ruff"
    :args `()
)

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
    (shell-command-to-string (format "ruff check --fix %s" temp-file))
    (erase-buffer)
    (insert-file-contents temp-file)
    (delete-file temp-file)
    (goto-char current-point)))
)

(defun ruff-fix-before-save ()
  (interactive)
  (when (memq major-mode '(python-mode python-ts-mode))
    (ruff-fix-buffer)))

(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :hook
  (python-mode . python-ts-mode)
  (python-mode . display-fill-column-indicator-mode)
  (python-mode . auto-fill-mode)
  (python-mode . (lambda()
		   (setq fill-column 79)))
  (python-mode . eglot-ensure)
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

(use-package treesit
  :config
  (setq treesit-font-lock-level 4)
)

;; (use-package tree-sitter
;;   :ensure t
;;   :hook
;;   (typescript-ts-mode . tree-sitter-hl-mode)
;;   (tsx-ts-mode . tree-sitter-hl-mode)
;;   :config
;;   (global-tree-sitter-mode)
;; )

(use-package tree-sitter-langs
  :ensure t
  ;; :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx))
  (tree-sitter-require 'rust)
  (add-to-list 'tree-sitter-major-mode-language-alist '(rust-ts-mode . rust))
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

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
)

;;; .emacs.el ends here
