;;; .emacs.el --- Initialization file for Emacs -*- lexical-binding: t: -*-

;;; Commentary:

;;; Code:

;; For debugging, uncomment 'profiler' in the beginning and the end of the config.
;; (require 'profiler)
;; (profiler-start 'cpu)

;; ===================================================
;; Packages
;; ===================================================
;; Create user-defined file so custom set variable/face are written to it.
(setq custom-file (expand-file-name "~/.emacs.d/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load the latest version of th all packages, making then available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages to be installed.
(defvar package-list
  '(
    use-package
    vertico
    consult
    flycheck
    company
    magit
    json-mode
    yaml-mode
    terraform-mode
    dockerfile-mode
    lsp-mode
    python-mode
    py-isort
    python-black
    js2-mode
    go-mode
    rust-mode
    markdown-mode))

;; Install them if the packages in 'package-list' are yet installed.
(dolist (p package-list)
  (when (not (package-installed-p p))
    (package-install p)))

;; ===================================================
;; Global config
;; ===================================================
;; Key mapping
;; (keyboard-translate ?\C-h ?\C-?) does not work in 'emacs --daemon'
;; cf. https://emacsWiki.org/emacs/BackspaceKey
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
				  (setq show-paren-delay 0)))

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

;;(setq mac-command-key-is-meta nil)
;;(setq mac-option-modifier 'meta)
;;(setq mac-command-modifier 'super)
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
;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; lsp
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind
  ("C-x C-e" . xref-find-definitions)
  ("C-x C-d" . xref-find-definitions-other-window)
  ("C-x C-r" . xref-find-references)
  ("C-x C-p" . xref-pop-marker-stack)
  :hook (lsp-mode . efs/lsp-mode-setup))


;; Python3
(defun lsp-set-python-cfg ()
    (let ((lsp-cfg '(:pyls (:configurationSources ("flake8")))))
      ;; TODO: check lsp--cur-workspace here to decide per server / project
      (lsp--set-configuration lsp-cfg)))

(use-package python-mode
  :ensure t
  :mode("\\.py\\'" . python-mode)
  :init
  (setq display-fill-column-indicator-column 79)
  :hook
  (python-mode . lsp-deferred)
  (python-mode . display-fill-column-indicator-mode)
  (lsp-after-initialize-hook . lsp-set-python-cfg))

(use-package py-isort
  :ensure t
  :config
  (setq py-isort-options '("--profile=black")))

(use-package python-black :ensure t)

(defun py-fmt()
  "Run isort and black."
  (interactive)
  (py-isort-buffer)
  (python-black-buffer))

;; (use-package python-mode
;;   :ensure t
;;   :mode("\\.py\\'" . python-mode)
;;   :init
;;   (setq display-fill-column-indicator-column 79)
;;   :init
;;   (setq-default indent-tabs-mode nil
;;                 display-fill-column-indicator-column 79)
;;   (setq tab-width 4)
;;   :config
;;   (setq python-indent-offset 4)
;;   :hook
;;   (python-mode . lsp-deferred)
;;   (python-mode . auto-fill-mode)
;;   (python-mode . display-fill-column-indicator-mode)
;;   (lsp-after-initialize-hook . lsp-set-python-cfg))



;; Golang
(defun lsp-go-before-save-hooks()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun go-fmt()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save t t))


(defun go-tab-width()
  (setq indent-tabs-mode nil)
  (setq tab-width 8))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook
  (go-mode . lsp-deferred)
  (go-mode . go-tab-width)
  (go-mode . go-fmt))
  ;; (go-mode . lsp-go-before-save-hooks))




;; Terraform
(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode)
  :hook
  (terraform-mode . lsp-deferred)
  (terraform-mode . terraform-format-on-save-mode))


;; ;; lsp
;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred)
;;   :after (:any go-mode rust-mode terraform-mode)
;;   :bind
;;   (("C-x C-e" . xref-find-definitions)
;;    ("C-x C-d" . xref-find-definitions-other-window)
;;    ("C-x C-r" . xref-find-references)
;;    ("C-x C-p" . xref-pop-marker-stack))
;;   :hook
;;   ;; For go-mode
;;   ((go-mode . lsp-deferred)
;;    (go-mode . lsp-go-before-save-hooks)


;;   ;; For rust-mode
;;   (rust-mode . lsp)
;;   ;; For terraform-mode
;;   (terraform-mode . lsp-deferred))
;;   :custom
;;   (lsp-rust-server 'rust-analyzer)
;;   (lsp-enable-snippet nil))
;;   :config
;;   (with-eval-after-load 'lsp-mode
;;     (lsp-register-client
;;       (make-lsp-client :new-connection (lsp-stdio-connection '("~/.go/bin/terraform-lsp" "serve"))
;;                        :major-modes '(terraform-mode)
;;                        :server-id 'terraform-ls)))


;;   :ensure t
;;   :mode("\\.py\\'" . python-mode)
;;   :init
;;   (setq-default indent-tabs-mode nil)
;;   :config
;;   (setq python-indent-offset 4)
;;   :hook
;;   ((python-mode . smartparens-mode)
;;    (python-mode . color-identifiers-mode))


;; Rust
(when (eq system-type 'darwin)
  (add-to-list 'exec-path (expand-file-name "/opt/homebrew/bin/rust-analyzer")))

(when (eq system-type 'gnu/linux)
  (add-to-list 'exec-path (expand-file-name "/usr/local/bin/rust-analyzer")))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
    ("C-c C-r" . 'rust-run)
    ("C-c C-t" . 'rust-test)
    ("C-c C-c" . 'rust-compile)
    ("C-c C-e" . 'rust-check))
  :hook
  (rust-mode . (lambda()
    (setq indent-tabs-mode nil)
    (setq rust-format-on-save t)
    (prettify-symbols-mode))))

;; Json
(use-package json-mode
  :ensure t
  :mode("\\.json\\'" . json-mode)
  :config
  (setq json-reformat:indent-width 2))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq-default indicate-empty-lines t))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Yaml
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

;; Docker
(use-package dockerfile-mode
  :commands (dockerfile-mode dockerfile-mode)
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; JS
(use-package js2-mode
  :ensure t
  :mode("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . (lambda()
    (setq indent-tabs-mode nil)
    (setq js2-basic-offset 2)
    (setq tab-width 2))))

;; typescript
;;(require 'typescript-mode)
;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;;(add-hook 'typescript-mode-hook
;;          (lambda ()
;;            (tide-setup)
;;            (flycheck-mode t)
;;            (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;            (eldoc-mode t)
;;            (company-mode-on)))

;;(profiler-report)
;;(profiler-stop)

;;; .emacs.el ends here
