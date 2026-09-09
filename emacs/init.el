;;; init.el --- Load MICE -*- lexical-binding: t; -*-

;; Change this path if mice.el is installed elsewhere.
(load "/home/hashylog/Documents/Projects/mice/mice.el")

;;; Personal visual settings

;; Load the darker Omtose Phellack variant installed from MELPA.
(load-theme 'omtose-darker t)
(require 'tab-line)

;; Reuse semantic faces from whichever theme is active.
(defun my-apply-ui-faces ()
  (let ((mode-line-background
         (face-attribute 'mode-line :background nil t))
        (inactive-background
         (face-attribute 'mode-line-inactive :background nil t))
        (muted-foreground
         (face-attribute 'shadow :foreground nil t)))
    (set-face-attribute 'mode-line nil
                        :inherit 'shadow
                        :foreground muted-foreground
                        :background mode-line-background
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :inherit 'shadow
                        :foreground muted-foreground
                        :background inactive-background
                        :box nil)
    (set-face-attribute 'tab-line nil
                        :inherit 'shadow
                        :foreground muted-foreground
                        :background inactive-background
                        :box nil)
    (set-face-attribute 'tab-line-tab-inactive nil
                        :inherit 'shadow
                        :foreground muted-foreground
                        :background inactive-background
                        :box nil)
    (set-face-attribute 'tab-line-tab-current nil
                        :inherit 'shadow
                        :foreground muted-foreground
                        :background mode-line-background
                        :weight 'bold
                        :box nil)))

(add-hook 'after-load-theme-hook #'my-apply-ui-faces)
(my-apply-ui-faces)

;; "File", "Edit", and similar entries belong to the menu bar.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Show only the current buffer name in the mode line.
(setq-default mode-line-format '("   %b"))

;; Show the buffers in the current window as native Emacs tabs.
(setq tab-line-close-button-show t
      tab-line-separator " "
      tab-line-new-button-show nil
      tab-line-switch-cycling t)
(global-tab-line-mode 1)


;;; init.el ends here
