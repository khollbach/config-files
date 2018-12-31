;; Don't garbage collect during startup, for faster startup.
;; Also, don't look too hard at the filenames loaded; this would waste time
;; evaluating regexes.
;; See https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; and http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

;;; Misc settings

;; Make the cursor not blink
(blink-cursor-mode 0)

;; Disable scroll bar and menu bars
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Disable startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(defun display-startup-echo-area-message ()
  (message ""))

;; No tabs please.
(setq-default indent-tabs-mode nil)

;
; TODO: not working?
;(setq _old_server-execute)
;(defun server-execute
;  (proc files nowait commands dontkill create-frame-func tty-name)
;  (progn
;    (_old_server-execute
;      proc files nowait commands dontkill create-frame-func tty-name)
;    (message "")))
;

;; Pause garbage collection on minibuffer setup. Supposedly helps things.
;; See http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)



;;; Package settings

;; Evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
;; jk -> ESC (relies on key-chord library).
;; See https://www.emacswiki.org/emacs/KeyChord
(load "~/.emacs.d/key-chord.el")
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)
;; Make Emacs treat underscore as a word character, as in Vim.
;; This way, motions like `w' and `e' work as expected.
(modify-syntax-entry ?_ "w")

;; Reset gc-cons-threshold and file-name-handler-alist.
)
