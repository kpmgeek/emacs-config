;;; early-init.el --- early bird init   -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; THIS IS A GENERATED FILE; changes should be made to README.org

;;; Code:

(setq load-prefer-newer t)

(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'auto-compile)
(auto-compile-on-load-mode +1)
(auto-compile-on-save-mode +1)

(setq use-package-verbose t)
(straight-use-package 'use-package)
(require 'bind-key)  ; needed for byte-compilation

(defconst *my/is-winnt*
  (or (string= "windows-nt" system-type)
      (string= "cygwin" system-type))
  "Non-nil if Emacs is running under Windows.")

(defconst *my/is-gnu-like*
  (or *my/is-winnt*  ; usually means GoW/Cygwin/MSYS2
      (string-prefix-p "gnu" (symbol-name system-type)))
  "Non-nil if we expect GNU-like coreutils.")

(defconst *my/is-macos*
  (memq window-system '(mac ns))
  "Non-nil if Emacs is running under macOS.")

(use-package no-littering)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(unless *my/is-macos*
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(add-to-list 'default-frame-alist
             '(font . "Fantasque Sans Mono-10"))

(set-face-attribute 'variable-pitch nil
                    :family "Source Serif Pro"
                    :height 1.25)

(use-package gruvbox-theme
  :demand t
  :config
  (setq my/light-theme 'gruvbox-light-medium)
  (setq my/dark-theme 'gruvbox-dark-soft)
  (setq my/initial-theme my/dark-theme)
  (load-theme my/initial-theme t))

(message "early bird init complete")

(provide 'early-init)
;;; early-init.el ends here
