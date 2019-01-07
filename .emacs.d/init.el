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
(setq linum-format "%3d")
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)

;; Visually indicate end-of-file, in a familiar style.
(load "~/.emacs.d/vi-tilde-fringe.el")
(require 'vi-tilde-fringe)
(add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)
(add-hook 'text-mode-hook 'vi-tilde-fringe-mode)



;;; Turn off various annoyances.

;; Disable bell sounds in MS Windows.
(setq ring-bell-function 'ignore)

;; Don't clutter the current directory with backup files,
;; prefer to save them in ~/.emacs.d/backups
;; See https://stackoverflow.com/a/151946
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Back up more intelligently.
;; See https://www.emacswiki.org/emacs/BackupFiles
(setq backup-by-copying t   ; don't clobber symlinks
      version-control t     ; use versioned backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Don't lock files when you edit them.
;; This would leave lockfiles scattered everywhere.
;; See https://www.emacswiki.org/emacs/LockFiles
;; TODO: not working, seeing #asdf# files sometimes, but not always ...
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
;; Installing Evil through a package-manager gets this automatically;
;; undo-tree is listed as a dependency of evil.

;; jk -> ESC (relies on key-chord package).
;; TODO: turn this into a key sequence instead of a chord.
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
;; TODO: see https://github.com/bling/emacs-evil-bootstrap
;; and C-h v evil-want-<TAB>
;; for possible alternatives.
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

;; C-e/C-y: scroll 5x faster.
;; TODO

;; Reset gc-cons-threshold and file-name-handler-alist.
)
