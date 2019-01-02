;; Don't garbage collect during startup, for faster startup.
;; Also, don't look too hard at the filenames loaded; this would waste time
;; evaluating regexes.
;; See https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; and http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

;;; ---------------------------------------------------------------------------
;;; Misc settings
;;; ---------------------------------------------------------------------------

;; Dark color theme.
(load-theme 'deeper-blue)

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

;; Pause garbage collection on minibuffer setup. Supposedly helps performance.
;; See http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;(defun my-minibuffer-setup-hook ()
  ;(setq gc-cons-threshold most-positive-fixnum))
;(defun my-minibuffer-exit-hook ()
  ;(setq gc-cons-threshold 800000))
;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)



;;; ---------------------------------------------------------------------------
;;; Package settings
;;; ---------------------------------------------------------------------------

;; Package management, repositories.
(require 'package)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Fetch package listing.
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages.
(let ((packages '(evil key-chord)))
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-install p))))



;; Evil
(evil-mode 1)

;; Vim-style linear undo/redo requires the undo-tree package installed.
;; (Specifically, undo-tree.el must be in the load-path.)
;; Installing through a package-manager gets this automatically; undo-tree is
;; listed as a dependency of evil.

;; jk -> ESC (relies on key-chord package).
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; Make Emacs treat underscore as a word character, as in Vim.
;; This way, motions like `w' and `e' work as expected.
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'text-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; normal mode C-u = page-up (Vim's C-u)
;; insert mode C-u = backward-kill-line (Readline's C-u)
;; M-u = universal-argument (Emacs' C-u)
;; See https://github.com/wasamasa/dotemacs/blob/master/init.org#evil
;; and https://www.emacswiki.org/emacs/BackwardKillLine
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(define-key global-map (kbd "C-u") 'backward-kill-line)
(define-key global-map (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

;; Reset gc-cons-threshold and file-name-handler-alist.
)
