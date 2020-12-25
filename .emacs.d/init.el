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

;; Use a dark theme.
;; Un-comment this if solarized-dark isn't available.
;(load-theme 'deeper-blue)

;; I'm not a huge fan of bold text in syntax highlighting. Disable it
;; everywhere.
;; https://stackoverflow.com/questions/2064904/how-to-disable-bold-font-weight-globally-in-emacs
;; https://www.reddit.com/r/emacs/comments/7r9b6d/disabling_bold_fonts_for_good/
(mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal))
   (face-list))

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

;; Hack to suppress the (useless) message "When done with this frame,
;; type C-x 5 0" that appears when launching emacs via `emacsclient`.
;; https://emacs.stackexchange.com/a/51763
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (setq inhibit-message t)
            (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil)))))

;; Show line numbers.
(setq linum-format "%3d ") ; Extra whitespace separating the line nums from the code.
(add-hook 'prog-mode-hook 'linum-mode)

;; Column number in mode line
(setq column-number-mode t)

;; Thin vertical line at 100 chars.
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook (lambda () (setq display-fill-column-indicator-column 100)))

;; Set font size to 14pt.
(set-face-attribute 'default nil :height 140)



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



;;; Defaults that make sense for programming.

;; For files that do not specify a major mode, be in text-mode by default.
(setq-default major-mode 'text-mode)

;; In text mode, wrap lines at word boundaries.
(add-hook 'text-mode-hook 'visual-line-mode)

;; Use spaces instead of hard tabs.
(setq-default indent-tabs-mode nil)

;; Remember my cursor location in files I've previously opened.
(save-place-mode 1)

;;; ---------------------------------------------------------------------------
;;; Package settings
;;; ---------------------------------------------------------------------------

;; Configure package manager.
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

;; Install `use-package' for managing other packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Solarized dark theme.
(use-package solarized-theme
  :ensure
  :config
  ;; Load this at most once. Otherwise, for some unknown reason, bold
  ;; fonts will be re-activated when you re-load it. (E.g., when
  ;; refreshing configs.)
  (unless (member 'solarized-dark custom-enabled-themes)
    (load-theme 'solarized-dark))
)

;; key-chord and key-seq, used for Evil insert-mode mappings.
(use-package key-chord
  :ensure
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
)
(use-package key-seq
  :ensure
  :after key-chord
)

;; Keybind for restarting emacs to get fresh configs.
(use-package restart-emacs
  :ensure
  :after key-seq evil
  :config
  ;(defun my-restart-emacs (arg)
  ;  (interactive "p")
  ;  (shell-command "~/config-files/update_configs")
  ;  (restart-emacs arg))
  ;(key-seq-define evil-motion-state-map " r" 'my-restart-emacs)

  (defun launch-separate-emacs-under-x ()
    (call-process "sh" nil nil nil "-c" " &")
    )
)  

;; Evil.
(use-package evil
  :ensure
  :demand
  :after key-seq
  :init
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1)

  ;; Make "insert mode" feel like emacs, i.e. give me RSI.
  ;; See https://stackoverflow.com/a/28985130/10994269
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  ;; jk -> ESC.
  (key-seq-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-seq-define evil-insert-state-map "jK" 'evil-normal-state)
  (key-seq-define evil-insert-state-map "Jk" 'evil-normal-state)
  (key-seq-define evil-insert-state-map "JK" 'evil-normal-state)

  ;; Same for replace mode.
  (key-seq-define evil-replace-state-map "jk" 'evil-normal-state)
  (key-seq-define evil-replace-state-map "jK" 'evil-normal-state)
  (key-seq-define evil-replace-state-map "Jk" 'evil-normal-state)
  (key-seq-define evil-replace-state-map "JK" 'evil-normal-state)

  ;; Make Emacs treat underscore as a word character, as in Vim.
  ;; This way, motions like `w' and `e' work as expected.
  (add-hook 'text-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

  ;; insert mode C-u = backward-kill-line (Readline's C-u)
  ;; M-u = universal-argument (Emacs' C-u)
  ;;
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
  (defun my-scroll-line-down (arg)
    (interactive "p")
    (evil-scroll-line-down (if (= arg 1) 5 arg)))
  (defun my-scroll-line-up (arg)
    (interactive "p")
    (evil-scroll-line-up (if (= arg 1) 5 arg)))
  (define-key evil-motion-state-map (kbd "C-e") 'my-scroll-line-down)
  (define-key evil-motion-state-map (kbd "C-y") 'my-scroll-line-up)

  ;; Switch panes.
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

  ;; Split panes.
  (define-key evil-motion-state-map (kbd "C-w j") 'evil-window-split)
  (define-key evil-motion-state-map (kbd "C-w k") 'evil-window-split)
  (define-key evil-motion-state-map (kbd "C-w h") 'evil-window-vsplit)
  (define-key evil-motion-state-map (kbd "C-w l") 'evil-window-vsplit)

  ;; Close the current pane.
  (key-seq-define evil-motion-state-map (kbd "SPC q") 'evil-quit)

  ;; Rebind help key from `C-h'.
  (global-set-key (kbd "M-h") help-map)

  ;; M-w: save the current file.
  (define-key global-map (kbd "M-w") 'save-buffer)

  ;; M-q: close the current Emacs client (a.k.a "frame").
  (define-key global-map (kbd "M-q") 'save-buffers-kill-terminal)

  ;;; Various key-bindings.

  ;; Unmap `SPC'.
  (defun do-nothing ()
    (interactive)
    nil)
  (define-key evil-motion-state-map " " 'do-nothing)

  ;; Reload definitions from `init.el`.
  (defun reload-configs ()
    (interactive)
    (shell-command "~/config-files/update_configs")
    (load-file "~/.emacs.d/init.el"))
  (key-seq-define evil-motion-state-map " e" 'reload-configs)

  ;; This properly kills emacs (including the `--daemon` process), so
  ;; your configs will be pristine next time you start it.  Make sure
  ;; you save config files *before* using this. Otherwise
  ;; `update_configs` won't see your changes.
  (defun update-configs-kill-emacs ()
    (interactive)
    (shell-command "~/config-files/update_configs")
    (save-buffers-kill-emacs))
  (key-seq-define evil-motion-state-map " r" 'update-configs-kill-emacs)

  ;; Edit `init.el`.
  (defun edit-configs ()
    (interactive)
    (find-file "~/config-files/.emacs.d/init.el"))
  ;; This doesn't accept "shift-space E", but I'm completed stumped on
  ;; how to do that.
  (key-seq-define evil-motion-state-map " E" 'edit-configs)
)

;; Rust syntax-highlighting, indentation, formatting.
(use-package rust-mode
  :ensure
  :config
  ;; Auto-format rust code on save.
  (setq rust-format-on-save t)
  ;; Run using `cargo run`.
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
)

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;(use-package lsp-mode
;  :ensure
;  :init (setq lsp-keymap-prefix "C-c C-l")
;  :hook ((rust-mode . lsp))
;  :commands lsp)


;; Reset gc-cons-threshold and file-name-handler-alist.
)
