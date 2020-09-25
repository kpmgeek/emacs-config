;;; init.el --- main init   -*- lexical-binding: t -*-

;;; Commentary:
;; THIS IS A GENERATED FILE; changes should be made to README.org

;;; Code:

; I guess this is useful for pre-27 Emacs, but it's mostly so that flycheck stops
; yelling at me.
(require 'early-init (expand-file-name "early-init.el" user-emacs-directory))

(defun my/tangle-init ()
  "Tangle the buffer if it's README.org in the user dir."
  (when (equal (buffer-file-name)
               (expand-file-name "README.org" user-emacs-directory))
    ;; Avoid running hooks when tangling
    (let ((prog-mode-hook nil)
          (org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (byte-compile-file (expand-file-name "init.el" user-emacs-directory)))))

(add-hook 'after-save-hook 'my/tangle-init)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(add-hook 'after-init-hook #'(lambda ()
                               (message "loading %s" custom-file)
                               (load custom-file t)))

(use-package exec-path-from-shell
  :if *my/is-macos*
  :config
  (exec-path-from-shell-initialize))

(setq backup-by-copying t    ; don't clobber symlinks
      version-control t      ; numbered backups
      delete-old-versions t  ; manage excess backups
      kept-old-versions 6
      kept-new-versions 9)

(setq delete-by-moving-to-trash t)

(when *my/is-macos*
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
     When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash")
                  nil 0 nil
                  file)))

(use-package savehist
  :custom
  (savehist-save-minibuffer-history t)
  (history-length 10000)  ; set to t for infinite history
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   shell-command-history))
  :config
  (savehist-mode +1))

(use-package recentf
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))

(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-unix)

(global-auto-revert-mode +1)

(global-display-line-numbers-mode -1)

(blink-cursor-mode -1)

(setq visible-bell t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode +1)

(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(show-paren-mode +1)

(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

(setq-default show-trailing-whitespace t)

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq show-trailing-whitespace nil)))

(setq-default fill-column 88)
(setq-default auto-fill-function 'do-auto-fill)

(setq-default truncate-lines nil)
(global-visual-line-mode 1)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(setq default-input-method "TeX")

(setq sentence-end-double-space nil)

(global-set-key (kbd "M-/") 'hippie-expand)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(add-hook 'after-init-hook #'server-start)

(setq apropos-do-all t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(defvar my/toggle-face-height-hook nil
  "Called when toggling the face height for mixed-DPI setups.")

(defun my/current-default-face-height ()
  "Get the height of the default face in the current frame."
  (face-attribute 'default :height (selected-frame)))

(defun my/toggle-face-height ()
  "Toggle the height of the default face in the current frame.
Useful when moving Emacs frames between monitors in mixed-DPI setups."
  (interactive)

  (set-face-attribute 'default (selected-frame) :height
                      (if (> (my/current-default-face-height) 80) 60 100))
  (run-hooks 'my/toggle-face-height-hook))

(global-set-key (kbd "C-x t s") 'my/toggle-face-height)

(use-package dash :config (dash-enable-font-lock))

(defun my/enable-compositions (ligatures)
  "Set up the `composition-function-table' for a list of LIGATURES."
  (-each (-group-by 'string-to-char ligatures)
    (-lambda ((char . comps))
      (set-char-table-range composition-function-table char
                            `([,(regexp-opt comps) 0 font-shape-gstring])))))

(defvar my/compositions
  '("!=" "!=="
    "==" "===" "=>" "==>" "=>>" "=/=" "=<<"
    "->" "-->" "->>" "-<" "-<<"
    "<-" "<-<" "<<-" "<--" "<->" "<=<" "<<=" "<==" "<=>" "<~~" "<~" "<<<"
    "<<" "<=" "<~>" "<>" "<|||" "<||" "<|" "<|>" "<!--"
    ">->" ">=>" ">>=" ">>-" ">-" ">=" ">>" ">>>"
    "~~" "~>" "~~>"
    "|>" "||>" "|||>" "||"
    "::" "&&"
    "//" "/*" "/**/"
    "*/"))
(my/enable-compositions my/compositions)

(global-prettify-symbols-mode -1)

(defun my/toggle-theme ()
  "Toggle between dark and light themes."
  (interactive)

  (let ((is-dark (seq-contains-p custom-enabled-themes my/dark-theme)))
    (-each custom-enabled-themes 'disable-theme)
    (load-theme (if is-dark my/light-theme my/dark-theme) t)))

(global-set-key (kbd "C-x t t") 'my/toggle-theme)

(use-package all-the-icons)

(use-package doom-modeline
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-height 40)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-minor-modes t)
  (doom-modeline-checker-simple-format nil)
  (doom-modeline-gnus nil)
  (doom-modeline-irc nil)
  :config
  (progn
    (doom-modeline-mode +1)
    (column-number-mode +1)))

(use-package delight)
(use-package emacs
  :delight
  (auto-fill-function " $")
  (visual-line-mode))

(use-package prescient
  :config (prescient-persist-mode +1))

(use-package hydra)

(use-package which-key
  :delight which-key-mode
  :config (which-key-mode +1))

(use-package company
  :demand t
  :delight company-mode
  :hook (after-init . global-company-mode))

(use-package company-prescient
  :config (company-prescient-mode +1))

(use-package smartparens
  :delight (smartparens-mode " ()")
  :hook (prog-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
  :demand t
  :delight ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  :config (ivy-mode +1))

(use-package counsel
  :after ivy
  :delight counsel-mode
  :config (counsel-mode +1))

(use-package ivy-prescient
  :after counsel
  :config (ivy-prescient-mode +1))

(use-package ivy-rich
  :after (ivy counsel all-the-icons-ivy-rich)
  :custom
  (ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1))

(use-package all-the-icons-ivy-rich
  :after counsel-projectile
  :config (all-the-icons-ivy-rich-mode +1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package hl-todo
  :demand t
  :config
  (progn
    (defun my/hl-todo-swiper ()
      "Search for TODOs in Swiper"
      (interactive)
      (swiper (substring (hl-todo--regexp) 2 -2)))

    (defhydra hydra-hl-todo (hl-todo-mode-map "C-c")
      "Search TODOs"
      ("N" hl-todo-previous "previous")
      ("n" hl-todo-next "next")
      ("s" my/hl-todo-swiper "swiper" :exit t)
      ("o" hl-todo-occur "occur" :exit t)
      ("i" hl-todo-insert "insert" :exit t))
    (global-hl-todo-mode +1)))

(use-package flycheck
  :demand t
  :delight flycheck-mode  ; doom-modeline has a dedicated indicator for this
  :hook (after-init . global-flycheck-mode))

(use-package projectile
  :delight (projectile-mode
            (:eval (format " p:%s" (projectile-project-type))))
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  :config (projectile-discover-projects-in-search-path))

(use-package counsel-projectile
  :config (counsel-projectile-mode +1))  ; also enables projectile-mode

(use-package undo-tree
  :delight undo-tree-mode
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :config (global-undo-tree-mode +1))

(use-package ace-window
  :demand t
  :custom
  (aw-display-mode-overlay nil)
  (aw-dispatch-always t)
  (aw-background nil)
  :bind ("M-o" . ace-window)
  :config (ace-window-display-mode +1))

(use-package treemacs
  :hook (after-init . treemacs-select-window)  ; open on start
  :config
  (progn
    (add-to-list 'aw-dispatch-alist '(?t treemacs-select-window))
    (add-to-list 'aw-dispatch-alist '(?T treemacs))
    (treemacs-git-mode 'deferred)
    (treemacs-filewatch-mode 1)
    (define-key treemacs-mode-map [mouse-1]
      #'treemacs-single-click-expand-action)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(add-hook 'my/toggle-face-height-hook
          #'(lambda ()
              (treemacs-resize-icons
               (if (> (my/current-default-face-height) 80) 22 11))))

(use-package treemacs-all-the-icons
  :requires all-the-icons)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package git-gutter-fringe
  :delight git-gutter-mode
  :config (global-git-gutter-mode +1))

(use-package evil
  :config
  (evil-mode +1))

(use-package evil-magit
  :after (evil magit))

(use-package origami
  :after evil
  :hook (magit-mode . (lambda () (origami-mode -1)))  ; don't enable this in magit
  :config
  (progn
    (evil-define-key 'normal origami-mode-map "zo" 'origami-open-node)
    (evil-define-key 'normal origami-mode-map "zO" 'origami-open-node-recursively)
    (evil-define-key 'normal origami-mode-map "zc" 'origami-close-node)
    (evil-define-key 'normal origami-mode-map "zC" 'origami-close-node-recursively)
    (evil-define-key 'normal origami-mode-map "za" 'origami-forward-toggle-node)
    (evil-define-key 'normal origami-mode-map "zA" 'origami-recursively-toggle-node)
    (evil-define-key 'normal origami-mode-map "zv" 'origami-show-node)
    (evil-define-key 'normal origami-mode-map "zx" 'origami-reset)
    (evil-define-key 'normal origami-mode-map "zm" 'origami-close-all-nodes)
    (evil-define-key 'normal origami-mode-map "zr" 'origami-open-all-nodes)
    (global-origami-mode +1)))

(use-package evil-cleverparens
  :delight evil-cleverparens-mode
  :hook (smartparens-enabled . evil-cleverparens-mode))

(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

(define-key evil-normal-state-map (kbd "/") 'swiper)
(define-key evil-normal-state-map (kbd "?") 'swiper-backward)
(define-key evil-normal-state-map (kbd "*") 'swiper-thing-at-point)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-l")
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy  ; ivy integration
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs  ; treemacs integration
  :commands lsp-treemacs-errors-list)

(delight 'eldoc-mode nil t)

(use-package irony
  :hook (((c++-mode c-mode objc-mode) . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :init
  (progn
    (when (string= "windows-nt" system-type)
      (setq exec-path (append exec-path '("~/scoop/apps/llvm/10.0.0/bin"))))
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))))

(use-package cmake-ide
  :if nil
  :demand t
  :config (cmake-ide-setup))

(use-package cuda-mode
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

(use-package fish-mode
  :mode (("\\.fish\\'" . fish-mode)))

;; (use-package company-go)
(use-package go-mode
  :mode ("\\.go\\'". go-mode)
  :init
  (progn
    (defun my/go-mode-locals ()
      ;; (set (make-local-variable 'company-backends) '(company-go))
      ;; (company-mode 1)
      (setq tab-width 3))
    (add-hook 'go-mode-hook #'my/go-mode-locals)
    (add-hook 'go-mode-hook #'flycheck-mode)
    (add-hook 'before-save-hook #'gofmt-before-save)))

(use-package lua-mode
  :commands (lua-mode)
  :mode ("\\.lua\\'" . lua-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package matlab-mode
  :mode "\\.m\\'"
  :init
  (progn
    (setq matlab-indent-function t)  ; TODO figure out what this does
    (setq matlab-shell-command "/usr/local/bin/matlab")))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-log-done t)
  (org-hide-emphasis-markers t))

(use-package htmlize
  :commands (htmlize-buffer
             htmlize-region
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired))

(delight 'buffer-face-mode nil t)
(use-package org-variable-pitch
  :delight
  (org-variable-pitch-minor-mode)
  :hook (org-mode . org-variable-pitch--enable)
  :config
  (set-face-attribute 'org-variable-pitch-fixed-face nil
                      :family (org-variable-pitch--get-fixed-font)
                      :height 0.8)
  (set-face-attribute 'org-level-1 nil :height (+ 1.0 (expt 0.5 0)))
  (set-face-attribute 'org-level-2 nil :height (+ 1.0 (expt 0.5 1)))
  (set-face-attribute 'org-level-3 nil :height (+ 1.0 (expt 0.5 2)))
  (set-face-attribute 'org-level-4 nil :height (+ 1.0 (expt 0.5 3)))
  (set-face-attribute 'org-level-5 nil :height (+ 1.0 (expt 0.5 4)))
  (set-face-attribute 'org-level-6 nil :height (+ 1.0 (expt 0.5 5)))
  (set-face-attribute 'org-level-7 nil :height (+ 1.0 (expt 0.5 6)))
  (set-face-attribute 'org-level-8 nil :height (+ 1.0 (expt 0.5 7))))

(use-package org-d20
  :commands org-d20-mode)

;; (defun my/ocaml/init-opam ()
;;   (if (executable-find "opam")
;;       (let ((share (string-trim-right
;;                     (with-output-to-string
;;                       (with-current-buffer
;;                           standard-output
;;                         (process-file
;;                          shell-file-name nil '(t nil) nil shell-command-switch
;;                          "opam config var share"))))))
;;         (cond ((string= "" share)
;;                (message "warning: `%s' output empty string." "opam config var share"))
;;               ((not (file-directory-p share))
;;                (message "%s" "warning: opam share directory does not exist."))
;;               (t (setq opam-share share
;;                        opam-load-path (concat share "/emacs/site-lisp"))
;;                  (add-to-list 'load-path opam-load-path))))
;;     (unless (executable-find "ocamlmerlin")
;;       (message "warning: cannot find `%s' or `%s' executable." "opam" "merlin"))))

(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :init
  (progn
    ;; (my/ocaml/init-opam)
    (add-hook 'tuareg-mode-hook 'company-mode)
    (add-hook 'tuareg-mode-hook 'flycheck-mode)
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt"
                   ".cmti" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))))

(use-package merlin
  :delight (merlin-mode " ⚗")
  :hook (tuareg-mode . merlin-mode)
  :init
  (progn
    (add-to-list 'company-backends 'merlin-company-backend)))

(use-package ocp-indent
  :hook (tuareg-mode . ocp-indent-caml-mode-setup))

(with-eval-after-load 'smartparens
  (sp-local-pair 'tuareg-mode "'" nil :actions nil)
  (sp-local-pair 'tuareg-mode "`" nil :actions nil))

(use-package utop
  :delight (utop-minor-mode " ū")
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (progn
    (if (executable-find "opam")
        (setq utop-command "opam config exec -- utop -emacs")
      (message "warning: cannot find `opam' executable."))))

(use-package flycheck-ocaml
  :after (flycheck merlin)
  :config
  (progn
    (setq merlin-error-after-save nil)
    (flycheck-ocaml-setup)))

(use-package dune
  :mode ("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\)?\\'" . dune-mode)
  :commands (dune-promote dune-runtest-and-promote))

(with-eval-after-load 'projectile
  (projectile-register-project-type
   'dune '("dune-project")
   :compile "dune build"
   :test "dune runtest"))

(use-package lsp-python-ms
  :defer t
  :custom
  (lsp-python-ms-auto-install-server t)
  (lsp-python-ms-executable (executable-find "Microsoft.Python.LanguageServer"))
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(use-package blacken
  :delight blacken-mode
  :hook (python-mode . blacken-mode))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode))

(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :custom
  (TeX-parse-self t) ; Enable parse on load.
  (TeX-auto-save t) ; Enable parse on save.
  (TeX-view-program-list
   '(("SumatraPDF"
      ("SumatraPDF.exe -reuse-instance"
       (mode-io-correlate " -forward-search \"%b\" %n")
       " %o")
      "SumatraPDF")))
  (TeX-view-program-selection '((output-pdf "SumatraPDF")))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex))

(use-package zig-mode
  :commands (zig-mode)
  :hook (zig-mode . lsp)
  :mode ("\\.zig\\'" . zig-mode))

(with-eval-after-load 'projectile
  (projectile-register-project-type
   'zig '("build.zig")
   :compile "zig build"
   :test "zig build"))

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (expand-file-name "~/Source/zls/zig-cache/bin/zls.exe"))
    :major-modes '(zig-mode)
    :server-id 'zls)))

(setq-default explicit-shell-file-name "/bin/bash")

(defconst *my/local-id*
  (format "%s.%s" (downcase (system-name)) system-type)
  "Hostname-based identifier for the current installation.")

(defvar my/local-config-count 0
  "The number of local configs that have been loaded.")

(defmacro my/config-for-local-id (id &rest body)
  "Run BODY only on the installation identified by ID."
  (declare (indent defun))
  `(when (string= ,id *my/local-id*)
     (setq my/local-config-count (1+ my/local-config-count))
     ,@body))

(my/config-for-local-id "ancalagon.gnu/linux"
  (setq projectile-project-search-path '("~/src"))
  (setq treemacs-python-executable (executable-find "python3")))

(my/config-for-local-id "galatine.windows-nt"
  (setq projectile-project-search-path '("~/Source"))
  (setq treemacs-python-executable (executable-find "python"))
  (setq flycheck-python-pycompile-executable (executable-find "python"))
  (setq ispell-program-name (expand-file-name "~/bin/hunspell-current/bin/hunspell.exe")))

(my/config-for-local-id "milliways.gnu/linux"
  (setq projectile-project-search-path '("~/src"))
  (setq treemacs-python-executable (executable-find "python3")))

(message "Loaded %d sections matching local id \"%s\""
         my/local-config-count *my/local-id*)
(message "main init complete")

(provide 'init)
;;; init.el ends here
