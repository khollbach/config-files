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
  :config
  ;; Load this at most once. Otherwise, for some unknown reason, bold
  ;; fonts will be re-activated when you re-load it. (E.g., when
  ;; refreshing configs.)
  (unless (member 'solarized-dark custom-enabled-themes)
    (load-theme 'solarized-dark))
)

;; Evil.
(use-package evil
  :config
  (evil-mode 1)

  ;; Remap most normal mode commands.
  (load-file "~/.emacs.d/my-configs/packages/evil-unmap.el")
  (load-file "~/.emacs.d/my-configs/packages/evil-remap.el")

  ;; Make Emacs treat underscore as a word character, as in Vim.
  ;; This way, motions like `w' and `e' work as expected.
  (add-hook 'text-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

  ;;; Various key-bindings.

  ;; Reload definitions from `init.el`.
  (defun reload-configs ()
    (interactive)
    (shell-command "~/.config-files/update_configs")
    (load-file "~/.emacs.d/init.el"))
  (key-seq-define evil-motion-state-map " e" 'reload-configs)

  ;; This properly kills emacs (including the `--daemon` process), so
  ;; your configs will be pristine next time you start it.  Make sure
  ;; you save config files *before* using this. Otherwise
  ;; `update_configs` won't see your changes.
  (defun update-configs-kill-emacs ()
    (interactive)
    (shell-command "~/.config-files/update_configs")
    (save-buffers-kill-emacs))
  (key-seq-define evil-motion-state-map " r" 'update-configs-kill-emacs)

  ;; Edit `init.el`.
  (defun edit-configs ()
    (interactive)
    (find-file "~/.config-files/.emacs.d/init.el"))
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
