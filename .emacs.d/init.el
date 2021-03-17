;; Don't garbage collect during startup, for faster startup.  Also,
;; don't look too hard at the filenames loaded; this would waste time
;; evaluating regexes.
;;
;; See https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; and http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))
  (load-file "~/.emacs.d/my-configs/init.el"))
