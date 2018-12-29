;; Make the cursor not blink
(blink-cursor-mode 0)

;; Disable startup messages
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ()
  (message ""))
(setq initial-scratch-message "")
; TODO:
;(setq _old_server-execute)
;(defun server-execute
;  (proc files nowait commands dontkill create-frame-func tty-name)
;  (progn
;    (_old_server-execute
;      proc files nowait commands dontkill create-frame-func tty-name)
;    (message "")))

;; Disable scroll bar and menu bars
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
