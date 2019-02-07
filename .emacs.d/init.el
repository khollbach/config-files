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

;;; Aesthetic changes.

;; Dark theme.
(load-theme 'deeper-blue)

;; Make the cursor not blink
(blink-cursor-mode 0)

;; Disable scroll bar and menu bars
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Disable startup messages
(setq inhibit-startup-screen t) ; Splash screen
(setq initial-scratch-message "") ; *scratch* buffer
(defun display-startup-echo-area-message () ; Echo area / minibuffer
  (message ""))

;; Show line numbers.
(setq linum-format "%3d ")
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)

;; Column number in mode line
(setq column-number-mode t)

;; Set font, size
(set-default-font "DejaVu Sans Mono 14")



;;; Turn off various annoyances.

;; Disable bell sounds in MS Windows.
(setq ring-bell-function 'ignore)

;; Open with current directory of ~ on Windows, instead of the dir containing
;; the Emacs binary.
(when (eq system-type 'windows-nt)
  (setq default-directory "~/"))

;; Don't clutter the current directory with backup files;
;; prefer to save them in `~/.emacs.d/backups`.
;; And use saner defaults for backups.
;; See https://stackoverflow.com/a/151946
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Don't create `.#asdf` files in the current directory.
(setq create-lockfiles nil)

;; Leave my init.el alone! Save Custom configs in their own file.
;; See http://emacsblog.org/2008/12/06/quick-tip-detaching-the-custom-file/
;; and Emacs help for `custom-file' variable.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)



;;; Misc.

;; For files that do not specify a major mode, be in text-mode by default.
(setq-default major-mode 'text-mode)

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

;; Configure package manager
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; key-chord
(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
)

;; Evil
(use-package evil
  :demand
  :after key-chord
  :init
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1)

  ;; Make "insert mode" feel like emacs, i.e. give me RSI.
  ;; See https://stackoverflow.com/a/28985130/10994269
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  ;; jk -> ESC (relies on key-chord package).
  ;; TODO: turn this into a key sequence instead of a chord.
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

  ;; Make Emacs treat underscore as a word character, as in Vim.
  ;; This way, motions like `w' and `e' work as expected.
  (add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'text-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

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

  ;; C-e/C-y: scroll 5x faster.
  ;; TODO
  ;(with-eval-after-load 'evil-maps
    ;(define-key evil-motion-state-map (kbd "C-e") 'evil-scroll-line-down)
    ;(define-key evil-motion-state-map (kbd "C-y") 'evil-scroll-line-up))
)

;; Scala syntax highlighting (and a bunch of other stuff I don't use)
(use-package ensime)

(use-package fill-column-indicator
  :init
  (setq fci-rule-column 80)
  :config
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode)
)

;; Reset gc-cons-threshold and file-name-handler-alist.
)
