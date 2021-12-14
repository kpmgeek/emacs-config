;;; init.el --- main init   -*- lexical-binding: t -*-

;;; Commentary:
;; THIS IS A GENERATED FILE; changes should be made to README.org

;;; Code:

; I guess this is useful for pre-27 Emacs, but it's mostly so that flycheck stops
; yelling at me.
(require 'early-init (expand-file-name "early-init.el" user-emacs-directory))

(use-package no-littering)

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

(use-package ispell
  :defer t
  :custom
  (ispell-program-name (if *my/is-winnt* "hunspell" "aspell"))
  (ispell-dictionary "en_US"))

(use-package flyspell
  :delight (flyspell-mode " ~")
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;(add-hook 'after-init-hook #'server-start)

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

(use-package dash :config (global-dash-fontify-mode))

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
    ;; "//"  ;; c++-mode hangs when this is enabled???
    "/*" "/**/"
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

(use-package page-break-lines
  :delight page-break-lines-mode
  :config (global-page-break-lines-mode +1))

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
  :hook ((prog-mode . smartparens-mode)
         (emacs-lisp-mode . smartparens-strict-mode))
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
  :bind (("<f7>" . projectile-compile-project)
         ("<f5>" . projectile-run-project))
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
  :bind ("C-x o" . ace-window)
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
  :requires all-the-icons
  :config (treemacs-load-theme 'all-the-icons))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-commit
  :hook (git-commit-mode . (lambda () (setq fill-column 72))))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package git-gutter-fringe
  :delight git-gutter-mode
  :config (global-git-gutter-mode +1))

(use-package git-modes
  :mode (("/\\.gitattributes\\'" . gitattributes-mode)
         ("/info/attributes\\'" . gitattributes-mode)
         ("/git/attributes\\'" . gitattributes-mode)
         ("/\\.gitconfig\\'" . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/etc/gitconfig\\'" . gitconfig-mode)
         ("/\\.gitignore\\'" . gitignore-mode)
         ("/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(defun my/c-c ()
  "Start a key sequence for a major mode command."
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd "C-c"))))

(defun my/c-x ()
  "Start a key sequence for a general command."
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd "C-x"))))

(use-package evil
  :demand t
  :bind (("C-x SPC" . counsel-M-x)      ; available as SPC SPC
         :map evil-normal-state-map
         ("SPC" . my/c-x)
         ("," . my/c-c))
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
  :hook (lisp-mode . evil-cleverparens-mode))

(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

(define-key evil-normal-state-map (kbd "/") 'swiper)
(define-key evil-normal-state-map (kbd "?") 'swiper-backward)
(define-key evil-normal-state-map (kbd "*") 'swiper-thing-at-point)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package restart-emacs
  :commands restart-emacs)

(use-package company-box
  :delight
  :hook (company-mode . company-box-mode))

(use-package window-purpose
  :bind (:map purpose-mode-map
              ("C-x b" . nil)
              ("C-x C-f" . nil))
  :config
  (purpose-mode +1)
  (require 'window-purpose-x)
  (purpose-x-magit-single-on))

(delight 'eldoc-mode nil t)

(defun my/indent-setup ()
  (c-set-offset 'arglist-intro '+))

(add-hook 'c-mode-hook #'my/indent-setup)
(add-hook 'c++-mode-hook #'my/indent-setup)

(use-package irony
  :hook (((c++-mode c-mode objc-mode) . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :init
  (when *my/is-winnt*
    (add-to-list 'exec-path (expand-file-name "~/scoop/apps/llvm/10.0.0/bin") t)

    ;; Suggested in the documentation to improve performance.
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))))

(use-package flycheck-irony
  :hook ((flycheck-mode . flycheck-irony-setup)))

(use-package company-irony
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony-eldoc
  :hook ((irony-mode . irony-eldoc)))

(use-package dhall-mode)

(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook 'fish_indent-before-save)))
  :mode (("\\.fish\\'" . fish-mode)
         ("/fish_funced\\..*\\'" . fish-mode))
  :interpreter ("fish" . fish-mode))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :after kubernetes)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5")))

(define-minor-mode my/writer-mode
  "Minor mode for writing prose."
  :init-value nil :lighter nil :global nil
  (if my/writer-mode
      (my/writer-mode--enable)
    (my/writer-mode--disable)))

(defface my/writer-mode-default-face
  '((t :inherit font-lock-comment-face
       :family "iA Writer Duo S"
       :height 1.20))
  "Default face for body text.")

(defface my/writer-mode-hl-line-face
  '((t :foreground "#ebdbb2"))
  "Default face for current line.")

(defun my/writer-mode--window-max-text-width (&optional window)
  "Return the maximum possible text width of WINDOW."
  (or window (setq window (selected-window)))
  (let* ((margins (window-margins window))
         (buffer (window-buffer window))
         (scale (if (and (boundp 'text-scale-mode-step)
                         (boundp 'text-scale-mode-amount))
                    (with-current-buffer buffer
                      (expt text-scale-mode-step
                            text-scale-mode-amount))
                  1.0)))
    (truncate (/ (+ (window-width window)
                    (or (car margins) 0)
                    (or (cdr margins) 0))
                 (float scale)
                 1.1))))

(defun my/writer-mode--adjust-window (&optional window)
  "Adjust the margins and fringes of WINDOW."
  (or window (setq window (selected-window)))
  (with-selected-window window
    (when my/writer-mode
      (set-window-fringes window nil nil t)
      (set-window-parameter window 'min-margins '(0 . 0))
      (let* ((total-width (my/writer-mode--window-max-text-width window))
             (margins (max 0 (- total-width fill-column))))
        (set-window-margins window (/ margins 2))))))

(defun my/writer-mode--enable ()
  "Set up `my/writer-mode' for the current buffer."
  (add-hook 'window-configuration-change-hook
            #'my/writer-mode--adjust-window 'append 'local)
  (add-hook 'window-state-change-functions
            #'my/writer-mode--adjust-window 'append 'local)
  (set (make-local-variable 'buffer-face-mode-face) 'my/writer-mode-default-face)
  (set (make-local-variable 'hl-line-face) 'my/writer-mode-hl-line-face)
  (buffer-face-mode +1)
  (hl-line-mode +1)
  (setq fill-column 70))

(defun my/writer-mode--disable ()
  "Disable `my/writer-mode' for the current buffer."
  (remove-hook 'window-configuration-change-hook
               #'my/writer-mode--adjust-window 'local)
  (remove-hook 'window-state-change-functions
               #'my/writer-mode--adjust-window 'local)
  (buffer-face-mode -1)
  (hl-line-mode -1)
  (setq fill-column (default-value 'fill-column))
  (let ((window (get-buffer-window (current-buffer))))
    (set-window-margins window 0 0)
    (set-window-parameter window 'min-margins nil)
    (set-window-fringes window nil)))

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

(use-package blacken
  :delight blacken-mode
  :hook (python-mode . blacken-mode))

(use-package geiser)
(use-package geiser-chicken)
(use-package geiser-guile
  :custom (geiser-guile-binary "guile3"))

(add-to-list 'load-path (expand-file-name "chicken" no-littering-etc-directory))
(use-package chicken
  :custom (geiser-chicken-binary "chicken-csi")
  :straight nil)

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

(my/config-for-local-id "caladbolg.berkeley-unix"
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

(setq-default explicit-shell-file-name "/bin/bash")

(defvar my/fortune "https://api.justyy.workers.dev/api/fortune")

(defun my/fortune ()
  "Insert a fortune from the web into the *scratch* buffer."
  (interactive)
  (let ((url-request-method "GET"))
    (url-retrieve
     my/fortune
     (lambda (status)
       (unless (plist-member status :error)
         (goto-char (point-min))
         (re-search-forward "^$")
         (let ((p (point)))
           (insert "[")
           (goto-char (point-max))
           (insert "]")
           (goto-char p))
         (let ((message (car (json-parse-buffer :array-type 'list))))
           (with-current-buffer "*scratch*"
             (goto-char (point-max))
             (let ((p (point)))
               (insert message)
               (comment-region p (point)))))
         (kill-buffer))))))

;; (my/fortune)

(add-to-list 'load-path (expand-file-name "groovy-splash" no-littering-etc-directory))
(use-package groovy-splash
  :straight nil
  :hook (after-init . groovy-splash-show)
  :custom
  (groovy-splash-segments '(groovy-splash-groovy-fill
                            groovy-splash-blank-line
                            groovy-splash-logo
                            groovy-splash-blank-line
                            groovy-splash-rule
                            groovy-splash-blank-line
                            groovy-splash-recentf
                            groovy-splash-blank-fill
                            groovy-splash-oracle
                            groovy-splash-blank-line)))

(use-package nasm-mode)
(add-to-list 'load-path (expand-file-name "noweb-mode" no-littering-etc-directory))
(use-package noweb-mode
  :straight nil)

(use-package forth-mode
  :straight nil
  :load-path "lib")

(message "Loaded %d sections matching local id \"%s\""
         my/local-config-count *my/local-id*)
(message "main init complete")

(provide 'init)
;;; init.el ends here
