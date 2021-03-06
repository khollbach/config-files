;;; Aesthetic changes.

;; Use a dark theme.
;; Un-comment this if solarized-dark isn't available.
;(load-theme 'deeper-blue)

;; Set font size to 14pt.
(set-face-attribute 'default nil :height 140)

;; Wrap lines at word boundaries.
(add-hook 'prog-mode-hook 'visual-line-mode)

;; Show line numbers.
(setq linum-format "%3d ") ; Extra whitespace separating the line nums from the code.
(add-hook 'prog-mode-hook 'linum-mode)

;; Column number in mode line
(setq column-number-mode t)

;; Thin vertical line at 100 chars.
;;
;; It would be nice to have one at 80 as well, but this feature doesn't seem to exist.
(add-hook 'prog-mode-hook (lambda ()
                            (display-fill-column-indicator-mode)
                            (setq display-fill-column-indicator-column 100)))



;;; Misc useful features.

;; Remember my cursor location in files I've previously opened.
(save-place-mode 1)

;; todo: it would be nice if command history persisted across sessions too,
;; e.g. things like `C-x C-f` file listings, so you can just press <up>
;; to see what you were recently looking at, etc, etc.

;; ``Smooth'' scrolling, instead of half-screen jumps.
;;
;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Insert comment characters.
;;
;; todo: this isn't working when I press <CR>
;;
;; https://stackoverflow.com/questions/11969442/comment-mode-in-emacs
;(defun my-prog-mode-hook ()
;    (setq fill-column 72)
;    (set (make-local-variable 'comment-auto-fill-only-comments) t)
;    (auto-fill-mode t))
;(add-hook 'prog-mode-hook 'my-prog-mode-hook)



;;; Turn off various annoyances.

;; I'm not a fan of bold text in syntax highlighting. Disable it
;; everywhere.
;;
;; todo: this isn't working right now (on Windows at least).
;;
;; https://stackoverflow.com/questions/2064904/how-to-disable-bold-font-weight-globally-in-emacs
;; https://www.reddit.com/r/emacs/comments/7r9b6d/disabling_bold_fonts_for_good/
(mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal))
   (face-list))

;; Don't blink the cursor.
(blink-cursor-mode 0)

;; Hide scroll bar, menu bars.
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Disable startup messages.
(setq inhibit-startup-screen t) ; Splash screen
(setq initial-scratch-message "") ; *scratch* buffer
(defun display-startup-echo-area-message () ; Echo area / minibuffer
  (message ""))

;; Hack to suppress the message "When done with this frame, type C-x 5
;; 0" that appears when launching emacs via `emacsclient`.
;;
;; https://emacs.stackexchange.com/a/51763
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (setq inhibit-message t)
            (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil)))))

;; Disable bell sounds in MS Windows.
(setq ring-bell-function 'ignore)

;; Open with current directory of ~ on Windows, instead of the dir containing
;; the Emacs binary.
(when (eq system-type 'windows-nt)
  (setq default-directory "~/"))

;; Don't clutter the current directory with backup files;
;; prefer to save them in `~/.emacs.d/backups`.
;; And use saner defaults for backups.
;;
;; https://stackoverflow.com/a/151946
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

;; Use spaces instead of hard tabs.
(setq-default indent-tabs-mode nil)

;; For files that do not specify a major mode, be in text-mode by default.
;; (The alternative is fundamental-mode, which isn't nearly as useful.)
(setq-default major-mode 'text-mode)

;; Disable electric-indent-mode when editing latex.
;; https://emacs.stackexchange.com/questions/30147/disabling-electric-indent-mode-for-one-mode-latex-mode-only
(defun remove-electric-indent-mode ()
  (electric-indent-local-mode -1))
(add-hook 'tex-mode-hook 'remove-electric-indent-mode)
