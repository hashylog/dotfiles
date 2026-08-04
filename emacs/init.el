;;; init.el --- Terminal-first VS Code/Micro workflow -*- lexical-binding: t; -*-

;; Keep Emacs' generated state out of this file and inside var/.
(defconst my/cache-directory (expand-file-name "var/" user-emacs-directory))
(defconst my/package-directory (expand-file-name "elpa/" my/cache-directory))
(dolist (directory (list my/cache-directory
                         my/package-directory
                         (expand-file-name "auto-save/" my/cache-directory)
                         (expand-file-name "auto-save-list/" my/cache-directory)
                         (expand-file-name "backups/" my/cache-directory)
                         (expand-file-name "transient/" my/cache-directory)))
  (make-directory directory t))

(setq package-user-dir my/package-directory
      package-quickstart-file (expand-file-name "package-quickstart.el" my/cache-directory)
      custom-file null-device
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my/cache-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" my/cache-directory) t))
      backup-directory-alist `(("." . ,(expand-file-name "backups/" my/cache-directory)))
      tramp-persistency-file-name (expand-file-name "tramp" my/cache-directory)
      recentf-save-file (expand-file-name "recentf" my/cache-directory)
      savehist-file (expand-file-name "savehist" my/cache-directory)
      save-place-file (expand-file-name "places" my/cache-directory)
      bookmark-default-file (expand-file-name "bookmarks" my/cache-directory)
      project-list-file (expand-file-name "projects" my/cache-directory)
      mc/list-file (expand-file-name "multiple-cursors.el" my/cache-directory)
      transient-levels-file (expand-file-name "transient/levels.el" my/cache-directory)
      transient-values-file (expand-file-name "transient/values.el" my/cache-directory)
      transient-history-file (expand-file-name "transient/history.el" my/cache-directory)
      url-configuration-directory (expand-file-name "url/" my/cache-directory))

(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" my/cache-directory)))

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
      message-log-max nil
      warning-minimum-level :error
      warning-minimum-log-level :error
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
      tab-width 4
      indent-tabs-mode nil
      require-final-newline t
      kill-do-not-save-duplicates t
      delete-by-moving-to-trash t
      uniquify-buffer-name-style 'forward
      vc-follow-symlinks t
      compilation-scroll-output 'first-error
      display-line-numbers-type 'relative)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(blink-cursor-mode -1)
(column-number-mode 1)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(global-so-long-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(winner-mode 1)
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

(defun my/remove-internal-log-buffers ()
  "Remove internal log buffers that are intentionally disabled."
  (dolist (name '("*Messages*" "*Warnings*" "*Async-native-compile-log*"))
    (when-let ((buffer (get-buffer name)))
      (kill-buffer buffer))))

(add-hook 'emacs-startup-hook #'my/remove-internal-log-buffers)

;; Shift plus an arrow extends the selection; arrows without Shift clear it.
(setq shift-select-mode t)

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local indent-tabs-mode nil))))
(dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook
                dired-mode-hook help-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'completion-preview-mode)

;;; Doom-inspired terminal UI

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :init
  (setq doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-position-column-line-format '(" %l:%c ")
        doom-modeline-percent-position '(" %p ")
        doom-modeline-project-name t
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
  (doom-modeline-mode 1))

(set-face-attribute 'vertical-border nil :foreground "#3f444a")
(set-face-attribute 'fringe nil :background "#282c34")
(set-face-attribute 'mode-line nil :background "#1b1d23" :foreground "#bbc2cf" :box nil)
(set-face-attribute 'mode-line-active nil :background "#1b1d23" :foreground "#bbc2cf" :box nil)
(set-face-attribute 'mode-line-inactive nil :background "#16181d" :foreground "#5B6268" :box nil)
(set-face-attribute 'doom-modeline-bar nil :background "#51afef")
(set-face-attribute 'doom-modeline-buffer-file nil :foreground "#dfdfdf" :weight 'bold)
(set-face-attribute 'doom-modeline-buffer-modified nil :foreground "#ECBE7B" :weight 'bold)
(set-face-attribute 'doom-modeline-project-dir nil :foreground "#98be65" :weight 'bold)
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

(use-package which-key
  :ensure nil
  :init
  (setq which-key-idle-delay 0.5
        which-key-max-description-length 40)
  :config
  (which-key-mode 1))

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

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-cycle t)
  (corfu-preselect 'prompt))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config (corfu-terminal-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package multiple-cursors)
(use-package clipetty
  :if (not (display-graphic-p))
  :config (global-clipetty-mode 1))

;;; Editor commands

(require 'project)
(require 'dired)
(require 'comint)
(require 'shell)

(defvar my/echo-area-clear-timer nil)

(defun my/clear-echo-area-later (text)
  "Clear informational echo-area messages shortly after displaying TEXT."
  (when (timerp my/echo-area-clear-timer)
    (cancel-timer my/echo-area-clear-timer))
  (when (and text (not (active-minibuffer-window)))
    (setq my/echo-area-clear-timer
          (run-at-time
           2 nil
           (lambda ()
             (unless (active-minibuffer-window)
               (message nil))))))
  nil)

(add-to-list 'set-message-functions #'my/clear-echo-area-later t)

(defun my/project-root ()
  "Return the current project root, falling back to `default-directory'."
  (if-let ((project (project-current)))
      (project-root project)
    default-directory))

(defun my/quick-open ()
  "Open a project file like VS Code's Quick Open."
  (interactive)
  (if (project-current)
      (call-interactively #'project-find-file)
    (call-interactively #'find-file)))

(defun my/new-buffer ()
  "Create a new untitled buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (funcall initial-major-mode)))

(defun my/close-buffer ()
  "Close the current buffer without closing its window."
  (interactive)
  (let* ((closing (current-buffer))
         (scratch-p (equal (buffer-name closing) "*scratch*"))
         (next (seq-find
                (lambda (buffer)
                  (and (not (eq buffer closing))
                       (buffer-live-p buffer)
                       (let ((name (buffer-name buffer)))
                         (and name
                              (not (string-prefix-p " " name))
                              (or (buffer-file-name buffer)
                                  (not (string-prefix-p "*" name)))))))
                (buffer-list))))
    (when (and (buffer-modified-p) (not scratch-p))
      (if (y-or-n-p (format "Save %s before closing? " (buffer-name)))
          (save-buffer)
        (set-buffer-modified-p nil)))
    (when scratch-p
      (set-buffer-modified-p nil))
    (kill-buffer closing)
    ;; Always keep one scratch buffer alive, but prefer another user buffer.
    (let* ((existing-scratch (get-buffer "*scratch*"))
           (scratch (or existing-scratch (get-buffer-create "*scratch*"))))
      (unless existing-scratch
        (with-current-buffer scratch
          (funcall initial-major-mode)))
      (switch-to-buffer (or next scratch)))))

(defun my/mouse-event-window (event)
  "Select the window under mouse EVENT, when it is a live window."
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (select-window window))))

(defun my/mouse-wheel-up (event)
  "Scroll three lines toward the beginning at mouse EVENT."
  (interactive "e")
  (my/mouse-event-window event)
  (condition-case nil
      (scroll-down-line 3)
    (beginning-of-buffer nil)))

(defun my/mouse-wheel-down (event)
  "Scroll three lines toward the end at mouse EVENT."
  (interactive "e")
  (my/mouse-event-window event)
  (condition-case nil
      (scroll-up-line 3)
    (end-of-buffer nil)))

(defun my/quit-emacs ()
  "Offer to save each file, then exit without a modified-buffer prompt."
  (interactive)
  (let ((confirm-kill-emacs nil))
    (save-some-buffers)
    (kill-emacs)))

(defun my/copy-region-or-line ()
  "Copy the active region or the current line."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-beginning-position 2))))
    (kill-ring-save start end)
    (deactivate-mark)))

(defun my/cut-region-or-line ()
  "Cut the active region or the current line."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-beginning-position 2))))
    (kill-region start end)))

(defun my/move-left ()
  "Move left and clear an active selection."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning)))
        (deactivate-mark)
        (goto-char start))
    (backward-char)))

(defun my/move-right ()
  "Move right and clear an active selection."
  (interactive)
  (if (use-region-p)
      (let ((end (region-end)))
        (deactivate-mark)
        (goto-char end))
    (forward-char)))

(defun my/move-up ()
  "Move up and clear an active selection."
  (interactive)
  (deactivate-mark)
  (previous-line))

(defun my/move-down ()
  "Move down and clear an active selection."
  (interactive)
  (deactivate-mark)
  (next-line))

(defun my/start-selection ()
  "Start a selection at point unless one is already active."
  (unless (use-region-p)
    (set-mark (point))
    (activate-mark)))

(defun my/select-all ()
  "Select the whole buffer with point at the end."
  (interactive)
  (set-mark (point-min))
  (goto-char (point-max))
  (activate-mark))

(defun my/select-left ()
  "Extend the selection one character left."
  (interactive)
  (my/start-selection)
  (backward-char))

(defun my/select-right ()
  "Extend the selection one character right."
  (interactive)
  (my/start-selection)
  (forward-char))

(defun my/select-up ()
  "Extend the selection one line up."
  (interactive)
  (my/start-selection)
  (previous-line))

(defun my/select-down ()
  "Extend the selection one line down."
  (interactive)
  (my/start-selection)
  (next-line))

(defun my/micro-mark-p (char)
  "Return non-nil when CHAR is a Unicode combining mark."
  (memq (and char (get-char-code-property char 'general-category))
        '(Mn Mc Me)))

(defun my/micro-word-char-p (char)
  "Return non-nil when CHAR belongs to a Micro-style word."
  (or (eq char ?_)
      (memq (and char (get-char-code-property char 'general-category))
            '(Lu Ll Lt Lm Lo Nd Nl No))))

(defun my/micro-whitespace-p (char)
  "Return non-nil when CHAR is whitespace according to Micro."
  (and char
       (or (memq char '(9 10 11 12 13 32 133 160 5760 8232 8233
                          8239 8287 12288))
           (and (<= 8192 char) (<= char 8202)))))

(defun my/micro-forward-character ()
  "Move over one character as represented internally by Micro."
  (unless (eobp)
    (forward-char)
    (while (and (not (eobp)) (my/micro-mark-p (char-after)))
      (forward-char))))

(defun my/micro-backward-character ()
  "Move back over one character as represented internally by Micro."
  (unless (bobp)
    (backward-char)
    (while (and (not (bobp)) (my/micro-mark-p (char-after)))
      (backward-char))))

(defun my/micro-next-character ()
  "Return the character following the Micro character at point."
  (save-excursion
    (my/micro-forward-character)
    (char-after)))

(defun my/micro-previous-character ()
  "Return the Micro character before point, clamped at line start."
  (save-excursion
    (unless (bolp)
      (my/micro-backward-character))
    (char-after)))

(defun my/micro-word-right (&optional keep-selection)
  "Move right using Micro's WordRight algorithm.
Preserve an active selection when KEEP-SELECTION is non-nil."
  (interactive)
  (when (and (not keep-selection) (use-region-p))
    (goto-char (region-end))
    (deactivate-mark))
  (catch 'done
    (when (eobp)
      (throw 'done nil))
    (when (eolp)
      (my/micro-forward-character)
      (throw 'done nil))
    (while (my/micro-whitespace-p (char-after))
      (when (eolp)
        (throw 'done nil))
      (my/micro-forward-character))
    (when (and (not (my/micro-word-char-p (char-after)))
               (not (my/micro-whitespace-p (char-after)))
               (not (my/micro-word-char-p (my/micro-next-character))))
      (while (and (not (my/micro-word-char-p (char-after)))
                  (not (my/micro-whitespace-p (char-after))))
        (when (eolp)
          (throw 'done nil))
        (my/micro-forward-character))
      (throw 'done nil))
    (my/micro-forward-character)
    (while (my/micro-word-char-p (char-after))
      (when (eolp)
        (throw 'done nil))
      (my/micro-forward-character))))

(defun my/micro-word-left (&optional keep-selection)
  "Move left using Micro's WordLeft algorithm.
Preserve an active selection when KEEP-SELECTION is non-nil."
  (interactive)
  (when (and (not keep-selection) (use-region-p))
    (goto-char (region-beginning))
    (deactivate-mark))
  (catch 'done
    (when (bobp)
      (throw 'done nil))
    (when (bolp)
      (my/micro-backward-character)
      (throw 'done nil))
    (my/micro-backward-character)
    (while (my/micro-whitespace-p (char-after))
      (when (bolp)
        (throw 'done nil))
      (my/micro-backward-character))
    (when (and (not (my/micro-word-char-p (char-after)))
               (not (my/micro-whitespace-p (char-after)))
               (not (my/micro-word-char-p (my/micro-previous-character))))
      (while (and (not (my/micro-word-char-p (char-after)))
                  (not (my/micro-whitespace-p (char-after))))
        (when (bolp)
          (throw 'done nil))
        (my/micro-backward-character))
      (my/micro-forward-character)
      (throw 'done nil))
    (my/micro-backward-character)
    (while (my/micro-word-char-p (char-after))
      (when (bolp)
        (throw 'done nil))
      (my/micro-backward-character))
    (my/micro-forward-character)))

(defun my/select-word-left ()
  "Extend the selection left using Micro's word movement."
  (interactive)
  (my/start-selection)
  (my/micro-word-left t)
  (activate-mark))

(defun my/select-word-right ()
  "Extend the selection right using Micro's word movement."
  (interactive)
  (my/start-selection)
  (my/micro-word-right t)
  (activate-mark))

(defun my/duplicate-line-or-region ()
  "Duplicate the active region or the current line."
  (interactive)
  (let* ((region (use-region-p))
         (start (if region (region-beginning) (line-beginning-position)))
         (end (if region (region-end) (line-beginning-position 2)))
         (text (buffer-substring start end)))
    (goto-char end)
    (insert text)
    (when region
      (set-mark end)
      (activate-mark))))

(defun my/move-line-up ()
  "Move the current line one line up."
  (interactive)
  (unless (bobp)
    (transpose-lines 1)
    (forward-line -2)))

(defun my/move-line-down ()
  "Move the current line one line down."
  (interactive)
  (forward-line 1)
  (unless (eobp)
    (transpose-lines 1)
    (forward-line -1)))

(defun my/toggle-explorer ()
  "Toggle a project-aware Dired sidebar."
  (interactive)
  (if-let ((window (seq-find (lambda (candidate)
                               (window-parameter candidate 'my-explorer))
                             (window-list))))
      (delete-window window)
    (let* ((buffer (dired-noselect (my/project-root)))
           (window (display-buffer-in-side-window
                    buffer '((side . left) (slot . -1) (window-width . 32)))))
      (set-window-parameter window 'my-explorer t)
      (set-window-dedicated-p window t)
      (with-current-buffer buffer
        (dired-hide-details-mode 1)
        (hl-line-mode 1))
      (select-window window))))

(defun my/toggle-terminal ()
  "Toggle an interactive shell in a bottom panel."
  (interactive)
  (let ((window (get-buffer-window "*terminal*")))
    (if window
        (delete-window window)
      (let ((buffer (get-buffer-create "*terminal*")))
        (unless (comint-check-proc buffer)
          (make-comint-in-buffer "terminal" buffer shell-file-name nil "-i")
          (with-current-buffer buffer (shell-mode)))
        (select-window
         (display-buffer-in-side-window
          buffer '((side . bottom) (slot . -1) (window-height . 0.30))))))))

(defun my/toggle-zen ()
  "Toggle a distraction-free single-window layout."
  (interactive)
  (if (> (length (window-list)) 1)
      (progn (winner-save-old-configurations) (delete-other-windows))
    (winner-undo)))

;;; Familiar Micro/VS Code keybindings

(global-set-key (kbd "C-s") #'save-buffer)
(global-set-key (kbd "C-S-s") #'write-file)
(global-set-key (kbd "C-n") #'my/new-buffer)
(global-set-key (kbd "C-q") #'my/close-buffer)
(global-set-key (kbd "M-q") #'my/quit-emacs)
(global-set-key (kbd "C-f") #'isearch-forward)
(global-set-key (kbd "C-z") #'undo-only)
(global-set-key (kbd "C-y") #'undo-redo)
(global-set-key (kbd "C-a") #'my/select-all)
(global-set-key (kbd "C-d") #'my/duplicate-line-or-region)
(global-set-key (kbd "C-/") #'comment-line)
(global-set-key (kbd "<left>") #'my/move-left)
(global-set-key (kbd "<right>") #'my/move-right)
(global-set-key (kbd "<up>") #'my/move-up)
(global-set-key (kbd "<down>") #'my/move-down)
(global-set-key (kbd "<S-left>") #'my/select-left)
(global-set-key (kbd "<S-right>") #'my/select-right)
(global-set-key (kbd "<S-up>") #'my/select-up)
(global-set-key (kbd "<S-down>") #'my/select-down)
(global-set-key (kbd "C-<left>") #'my/micro-word-left)
(global-set-key (kbd "C-<right>") #'my/micro-word-right)
(global-set-key (kbd "C-S-<left>") #'my/select-word-left)
(global-set-key (kbd "C-S-<right>") #'my/select-word-right)
(global-set-key [wheel-up] #'my/mouse-wheel-up)
(global-set-key [wheel-down] #'my/mouse-wheel-down)
(global-set-key [mouse-4] #'my/mouse-wheel-up)
(global-set-key [mouse-5] #'my/mouse-wheel-down)
(global-set-key (kbd "M-<up>") #'my/move-line-up)
(global-set-key (kbd "M-<down>") #'my/move-line-down)
(global-set-key (kbd "M-S-<up>") #'mc/mark-previous-like-this)
(global-set-key (kbd "M-S-<down>") #'mc/mark-next-like-this)
(global-set-key (kbd "C-SPC") #'completion-at-point)
(global-set-key (kbd "<f1>") help-map)
(global-set-key (kbd "<f12>") #'xref-find-definitions)
(global-set-key (kbd "S-<f12>") #'xref-find-references)
(global-set-key (kbd "C-e") #'execute-extended-command)
(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-S-p"))

;; These keys are intentionally left empty until they are remapped later.
(dolist (key '("C-o" "C-p" "C-w" "<f3>" "<S-f3>" "C-b" "C-`"
               "C-<tab>" "C-S-<tab>" "C-<prior>" "C-<next>" "C-," "C-S-g"))
  (global-unset-key (kbd key)))

;; A minor-mode map takes precedence over major-mode C-c prefixes. This makes
;; C-c and C-x conventional copy/cut keys everywhere, as requested.
(defvar-keymap my/familiar-keys-map
  "C-c" #'my/copy-region-or-line
  "C-x" #'my/cut-region-or-line
  "C-v" #'yank
  "C-q" #'my/close-buffer
  "M-q" #'my/quit-emacs
  "C-e" #'execute-extended-command
  "C-a" #'my/select-all)
(define-minor-mode my/familiar-keys-mode
  "Use conventional copy and cut keys instead of Emacs prefix maps."
  :global t
  :keymap my/familiar-keys-map)
(defvar my/familiar-keys-emulation-alist
  `((my/familiar-keys-mode . ,my/familiar-keys-map)))
(add-to-list 'emulation-mode-map-alists 'my/familiar-keys-emulation-alist)
(my/familiar-keys-mode 1)

;; While Find is active, arrows cycle through matches and RET accepts one.
(define-key isearch-mode-map (kbd "<down>") #'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<up>") #'isearch-repeat-backward)

;; VS Code-style C-k chords. M-x always remains available as an escape hatch.
(define-prefix-command 'my/code-prefix)
(global-set-key (kbd "C-k") 'my/code-prefix)
(define-key my/code-prefix (kbd "C-s") #'describe-bindings)
(define-key my/code-prefix (kbd "C-z") #'my/toggle-zen)
(define-key my/code-prefix (kbd "C-c") #'comment-region)
(define-key my/code-prefix (kbd "C-u") #'uncomment-region)
(define-key my/code-prefix (kbd "C-0") #'delete-window)
(define-key my/code-prefix (kbd "C-\\") #'split-window-right)
(define-key my/code-prefix (kbd "C--") #'split-window-below)

;; Web/configuration files conventionally use two spaces; EditorConfig wins
;; whenever a project supplies its own policy.
(dolist (hook '(js-mode-hook js-ts-mode-hook typescript-ts-mode-hook
                json-ts-mode-hook css-mode-hook css-ts-mode-hook
                html-mode-hook mhtml-mode-hook yaml-ts-mode-hook))
  (add-hook hook (lambda () (setq-local tab-width 2))))

(my/remove-internal-log-buffers)

(provide 'init)
;;; init.el ends here
