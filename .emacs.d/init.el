;; Make the cursor not blink
(blink-cursor-mode 0)

;; Disable startup messages
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ()
  (message ""))
(setq initial-scratch-message "")

;; Disable scroll bar and menu bars
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
