;; ============================================
;; Emacs Configuration
;; ============================================

;; Add local packages folder to load-path
(let ((default-directory (expand-file-name "packages" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; Disable menus and toolbars (for terminal mode)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Show line and column numbers
(global-display-line-numbers-mode t)
(column-number-mode t)

;; Disable startup messages
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Enable mouse in terminal mode
(xterm-mouse-mode 1)

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
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)

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
(setq indent-line-function 'insert-tab)

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
;; END OF CONFIGURATION
;; ============================================
