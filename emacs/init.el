;;; init.el --- Load MICE -*- lexical-binding: t; -*-

;; Change this path if mice.el is installed elsewhere.
(load "/home/hashylog/Documents/Projects/mice/mice.el")

;; Enable tab line
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

;; Show line numbers in all buffers.
(global-display-line-numbers-mode 1)


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b6c43bb2aea78890cf6bd4a970e6e0277d2daf0075272817ea8bb53f9c6a7f0a"
     default))
 '(package-selected-packages
   '(ample-theme clipetty consult dtrt-indent multiple-cursors orderless)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
