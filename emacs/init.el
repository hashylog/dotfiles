;;; init.el --- Terminal-first VS Code/Micro workflow -*- lexical-binding: t; -*-

;; Keep Emacs' generated state out of this file and inside var/.
(defconst my/cache-directory (expand-file-name "var/" user-emacs-directory))
(defconst my/package-directory (expand-file-name "elpa/" my/cache-directory))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(dolist (directory (list my/cache-directory
                         my/package-directory
                         (expand-file-name "auto-save/" my/cache-directory)
                         (expand-file-name "auto-save-list/" my/cache-directory)
                         (expand-file-name "backups/" my/cache-directory)
                         (expand-file-name "eln-cache/" my/cache-directory)))
  (make-directory directory t))

(setq package-user-dir my/package-directory
      custom-file null-device
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my/cache-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" my/cache-directory) t))
      backup-directory-alist `(("." . ,(expand-file-name "backups/" my/cache-directory)))
      tramp-persistency-file-name (expand-file-name "tramp" my/cache-directory)
      recentf-save-file (expand-file-name "recentf" my/cache-directory)
      savehist-file (expand-file-name "savehist" my/cache-directory)
      bookmark-default-file (expand-file-name "bookmarks" my/cache-directory)
      project-list-file (expand-file-name "projects" my/cache-directory)
      mc/list-file (expand-file-name "multiple-cursors.el" my/cache-directory)
      url-configuration-directory (expand-file-name "url/" my/cache-directory))

(when (boundp 'native-comp-eln-load-path)
  (let ((cache (expand-file-name "eln-cache/" my/cache-directory))
        (default-cache (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq native-comp-eln-load-path
          (cons cache (delete default-cache native-comp-eln-load-path)))))

;;; Packages

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 30) ("nongnu" . 20) ("melpa" . 10)))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;; Core behavior

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      ring-bell-function #'ignore
      use-dialog-box nil
      use-file-dialog nil
      confirm-kill-emacs #'y-or-n-p
      sentence-end-double-space nil
      scroll-conservatively 101
      scroll-margin 2
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      redisplay-dont-pause t
      fast-but-imprecise-scrolling t
      native-comp-jit-compilation nil
      read-process-output-max (* 1024 1024)
      kill-do-not-save-duplicates t
      delete-by-moving-to-trash t
      uniquify-buffer-name-style 'forward
      vc-follow-symlinks t
      compilation-scroll-output 'first-error
      display-line-numbers-type t)

(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil
              require-final-newline t)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(blink-cursor-mode -1)
(column-number-mode -1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(global-so-long-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(save-place-mode -1)
(savehist-mode 1)
(recentf-mode 1)
(tab-bar-mode -1)
(global-tab-line-mode 1)
(when (fboundp 'xterm-mouse-mode)
  (xterm-mouse-mode 1))
(when (fboundp 'mouse-wheel-mode)
  (mouse-wheel-mode 1))

;; Keep implementation buffers available to Emacs without ever displaying
;; them as editor tabs or automatic fallback buffers.
(dolist (pattern '("\\`\\*Messages\\*\\'"
                   "\\`\\*Warnings\\*\\'"
                   "\\`\\*Async-native-compile-log\\*\\'"))
  (add-to-list 'display-buffer-alist
                `(,pattern (display-buffer-no-window) (allow-no-window . t))))

(setq switch-to-prev-buffer-skip-regexp
      "\\` \\|\\`\\*\\(?:Messages\\|Warnings\\|Async-native-compile-log\\)\\*\\'")

(setq shift-select-mode t)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Doom-inspired terminal UI

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-percent-position nil
        doom-modeline-project-name nil
        doom-modeline-workspace-name nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-vcs-icon nil
        doom-modeline-vcs-max-length 24
        doom-modeline-check nil
        doom-modeline-lsp nil
        doom-modeline-modal nil
        doom-modeline-height 25)
  :config
  (setq-default mode-line-percent-position nil)
  (doom-modeline-mode 1))

(set-face-attribute 'vertical-border nil :foreground "#3f444a")
(set-face-attribute 'fringe nil :background "#282c34")
(set-face-attribute 'mode-line nil :background "#1b1d23" :foreground "#bbc2cf" :box nil)
(set-face-attribute 'mode-line-active nil :background "#1b1d23" :foreground "#bbc2cf" :box nil)
(set-face-attribute 'mode-line-inactive nil :background "#16181d" :foreground "#5B6268" :box nil)
(set-face-attribute 'doom-modeline-bar nil :background "#51afef")
(set-face-attribute 'doom-modeline-buffer-file nil :foreground "#dfdfdf" :weight 'bold)
(set-face-attribute 'doom-modeline-buffer-modified nil :foreground "#ECBE7B" :weight 'bold)
(set-face-attribute 'doom-modeline-buffer-major-mode nil :foreground "#c678dd" :weight 'bold)

(defun my/tab-line-tab-name (buffer &optional _buffers)
  "Return a compact terminal-friendly label for BUFFER."
  (with-current-buffer buffer
    (format " %s%s "
            (truncate-string-to-width (buffer-name) 24 nil nil "...")
            (if (buffer-modified-p) " *" ""))))

(set-face-attribute 'tab-line nil :background "#16181d" :foreground "#5B6268" :height 0.95 :box nil)
(set-face-attribute 'tab-line-tab-current nil :background "#51afef" :foreground "#1b1d23" :weight 'bold :box nil)
(set-face-attribute 'tab-line-tab-inactive nil :background "#21242b" :foreground "#73797e" :box nil)
(set-face-attribute 'tab-line-highlight nil :background "#3f444a" :foreground "#bbc2cf" :box nil)
(setq tab-line-close-button-show nil
      tab-line-new-button-show nil
      tab-line-separator ""
      tab-line-tab-name-function #'my/tab-line-tab-name
      tab-line-switch-cycling t
      window-divider-default-right-width 1)
(window-divider-mode 1)

;;; Completion and navigation

(use-package vertico
  :init
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (("C-S-f" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("M-y" . consult-yank-pop)))

(use-package dtrt-indent
  :init
  (setq dtrt-indent-lighter nil
        dtrt-indent-verbosity 0
        dtrt-indent-run-after-smie t)
  :config
  (dtrt-indent-global-mode 1))

(use-package multiple-cursors)

(use-package clipetty
  :if (not (display-graphic-p))
  :config
  (global-clipetty-mode 1))

(use-package treemacs
  :bind (("C-b" . treemacs))
  :init
  (setq treemacs-width 32
        treemacs-position 'left
        treemacs-show-hidden-files t
        treemacs-is-never-other-window nil)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(require 'my-editor-commands)
(require 'my-micro)
(require 'my-keybindings)
(require 'my-treemacs)

(provide 'init)
;;; init.el ends here

