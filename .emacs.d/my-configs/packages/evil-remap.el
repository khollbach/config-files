;; todo: unbind certain keys in Dired.
;; ...

;; Unmap space.
(define-key evil-motion-state-map " " 'ignore)

;; Save and quit.
(define-key global-map (kbd "M-s") 'save-buffer)
(define-key global-map (kbd "M-a") 'save-buffers-kill-terminal)

;; C-u kills to the beginning of the line, like it does in GNU `readline`.
(define-key evil-insert-state-map (kbd "C-u")
  '(lambda ()
     (interactive)
     (if (bound-and-true-p eshell-mode)
         (eshell-kill-input)
       (kill-line 0))))

;; hjkl
(define-key evil-motion-state-map "l" 'evil-backward-char)
(define-key evil-motion-state-map "n" 'evil-next-line)
(define-key evil-motion-state-map "e" 'evil-previous-line)
(define-key evil-motion-state-map "u" 'evil-forward-char)

;; Insert
(define-key evil-normal-state-map "s" 'evil-insert)
(define-key evil-normal-state-map "S" 'evil-insert-line)
(define-key evil-normal-state-map "o" 'evil-append)
(define-key evil-normal-state-map "O" 'evil-append-line)
(define-key evil-normal-state-map "a" 'evil-open-below)
(define-key evil-normal-state-map "A" 'evil-open-above)

;; Words
(define-key evil-motion-state-map "f" 'evil-backward-word-begin)
(define-key evil-motion-state-map "F" 'evil-backward-WORD-begin)
(define-key evil-motion-state-map "p" 'evil-forward-word-begin)
(define-key evil-motion-state-map "P" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map "i" 'evil-forward-word-end)
(define-key evil-motion-state-map "I" 'evil-forward-WORD-end)

;; Paragraphs, home/end
(define-key evil-motion-state-map "," 'evil-backward-paragraph)
(define-key evil-motion-state-map "." 'evil-forward-paragraph)
(define-key evil-motion-state-map "q" 'evil-first-non-blank)
(define-key evil-motion-state-map "Q" 'evil-beginning-of-line)
(define-key evil-motion-state-map ";" 'evil-end-of-line)
(define-key evil-motion-state-map ":" 'evil-ex)

;; Copy/paste
(define-key evil-motion-state-map "d" 'evil-yank)
(define-key evil-motion-state-map "r" 'evil-delete)
(define-key evil-motion-state-map "w" 'evil-change)
(define-key evil-motion-state-map "c" 'evil-paste-after)
(define-key evil-motion-state-map "C" 'evil-paste-before)

;; Find
;; todo...

;; Scrolling
(define-key evil-motion-state-map "h" 'evil-scroll-up)
(define-key evil-motion-state-map "t" 'evil-scroll-down)
(define-key evil-motion-state-map "H" 'evil-scroll-page-up)
(define-key evil-motion-state-map "T" 'evil-scroll-page-down)
(define-key evil-motion-state-map "N" 'scroll-5-lines-down)
(define-key evil-motion-state-map "E" 'scroll-5-lines-up)
(defun scroll-5-lines-down (arg)
  (interactive "p")
  (evil-scroll-line-down (if (= arg 1) 5 arg)))
(defun scroll-5-lines-up (arg)
  (interactive "p")
  (evil-scroll-line-up (if (= arg 1) 5 arg)))

;; Undo, replace, join, repeat
(define-key evil-normal-state-map "k" 'undo)
(define-key evil-normal-state-map "K" 'redo)
(define-key evil-normal-state-map "b" 'evil-replace)
(define-key evil-normal-state-map "B" 'evil-replace-state)
(define-key evil-normal-state-map "L" 'evil-join)
(define-key evil-normal-state-map (kbd "TAB") 'evil-repeat)

;; Visual
(define-key evil-motion-state-map "m" 'evil-visual-line)
(define-key evil-motion-state-map "M" 'evil-visual-char)
(define-key evil-motion-state-map (kbd "C-c") 'evil-visual-block)

;; Macros
(define-key evil-motion-state-map "v" 'evil-record-macro)
(define-key evil-motion-state-map "V" 'evil-execute-macro)
;; todo: get this to be the vim command `@@`:
;;(define-key evil-motion-state-map (kbd "DEL") 'evil-)

;; Cursor top/mid/bot
(define-key evil-motion-state-map "D" 'evil-window-top)
(define-key evil-motion-state-map "R" 'evil-window-middle)
(define-key evil-motion-state-map "W" 'evil-window-bottom)

;; Misc.
(define-key evil-motion-state-map (kbd "RET") 'evil-goto-first-line)
(define-key evil-motion-state-map "zn" 'evil-scroll-line-to-bottom)
(define-key evil-motion-state-map "ze" 'evil-scroll-line-to-top)
(define-key evil-motion-state-map "j" 'evil-search-next)
(define-key evil-motion-state-map "J" 'evil-search-previous)
(define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-line-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-line-up)

;; Jump to definition; jump back.
(define-key evil-motion-state-map (kbd "C-d") 'pop-tag-mark)
(define-key evil-motion-state-map (kbd "C-f") 'evil-jump-to-tag)



;;; Window management.

;; Switch panes.
(define-key evil-motion-state-map (kbd "M-m") 'evil-window-left)
(define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "M-i") 'evil-window-right)

;; Split panes.
(define-key evil-motion-state-map (kbd "C-w l") 'evil-window-vsplit)
(define-key evil-motion-state-map (kbd "C-w n") 'evil-window-split)
(define-key evil-motion-state-map (kbd "C-w e") 'evil-window-split)
(define-key evil-motion-state-map (kbd "C-w u") 'evil-window-vsplit)
