;; ============================================
;; Emacs Configuration
;; ============================================

;; Add local packages folder to load-path
(let ((default-directory (expand-file-name "packages" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Disable menus and toolbars (for terminal mode)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Show line and column numbers
(global-display-line-numbers-mode t)
(column-number-mode t)

;; Disable startup messages
;;(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Enable mouse in terminal mode
(xterm-mouse-mode 1)

;; Disable Custom File
(setq custom-file "/dev/null") 

;; ============================================
;; KEYBINDS
;; ============================================

;; Enable CUA mode with custom behavior
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; CTRL+S - Save buffer
(global-set-key (kbd "C-s") 'save-buffer)

;; CTRL+Q - Quit Emacs
(global-set-key (kbd "C-q") 'better-quit-emacs)

;; CTRL+F - Search (isearch-forward)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

;; CTRL+H - Search and replace
(global-set-key (kbd "C-h") 'query-replace)

;; CTRL+Z - Undo
(global-set-key (kbd "C-z") 'undo)

;; CTRL+Y - Redo
(global-set-key (kbd "C-y") 'undo-redo)

;; CTRL+A - Select all
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; CTRL+O - Open file
(global-set-key (kbd "C-o") 'find-file)

;; CTRL+W - Close buffer
(global-set-key (kbd "C-w") 'kill-buffer)

;; CTRL+N - New empty file
(global-set-key (kbd "C-n") 'new-empty-buffer)

;; CTRL+D - Duplicate line
(global-set-key (kbd "C-d") 'duplicate-line)

;; CTRL+K - Delete entire line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; CTRL+/ - Comment / Uncomment line
(global-set-key (kbd "C-/") 'comment-line)

;; ALT+UP/DOWN - Move line up/down
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; CTRL+G - Go to line
(global-set-key (kbd "C-g") 'goto-line)

;; CTRL+E - Execute command (M-x)
(global-set-key (kbd "C-e") 'execute-extended-command)

;; CTRL+P - Quick open (simple command palette)
(global-set-key (kbd "C-p") 'find-file)

;; TAB and SHIFT+TAB - Indent / Un-indent
(global-set-key (kbd "TAB") 'indent-for-tab-command)
(global-set-key (kbd "<backtab>") 'un-indent-region)

;; ============================================
;; AUXILIARY FUNCTIONS
;; ============================================

;; Quit Emacs
(defun better-quit-emacs (&optional arg)
  "Quit emacs without asking twice for saving."
  (interactive "P")
  (save-some-buffers arg)
  (kill-emacs))

;; Create a new empty buffer
(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

;; Duplicate the current line
(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column col)))

;; Move current line up
(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

;; Move current line down
(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; Un-indent region or current line
(defun un-indent-region ()
  "Remove one indentation level from the selected region or current line."
  (interactive)
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) (- tab-width))
    (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))))

;; ============================================
;; EDITING SETTINGS
;; ============================================

;; Tabs and spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent nil)

;; Force TAB to always insert spaces/tab, never indent
(setq-default indent-line-function 'insert-tab)

;; Disable electric indentation (auto-indent on newline)
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))

;; Make TAB key always insert tab/spaces in all modes
(defun my-insert-tab ()
  "Insert a tab character or spaces."
  (interactive)
  (insert-tab))

;; Override TAB behavior in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "TAB") 'my-insert-tab)))

;; Disable mode-specific indentation functions
(setq-default indent-line-function 'insert-tab)
;; Highlight matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0)

;; Delete selection when typing
(delete-selection-mode t)

;; Highlight current line (optional)
;; (global-hl-line-mode t)

;; Enable visual line wrapping
(global-visual-line-mode t)

;; Disable auto-save and backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; UTF-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; ============================================
;; VISUAL ENHANCEMENTS (Terminal)
;; ============================================

;; Theme
;;(load-theme 'wombat t)

;; Better syntax highlighting
(global-font-lock-mode t)

;; Custom mode-line format
(setq-default mode-line-format
              '("%e"
                " > "
                mode-line-buffer-identification
                " | "
                mode-name
                " | "
                mode-line-position))

;; ============================================
;; PACKAGES
;; ============================================

;; Custom Splash Screen
;; https://github.com/rougier/emacs-splash
(require 'splash-screen)


;; Treemacs File Manager
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   0
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; ============================================
;; END OF CONFIGURATION
;; ============================================

