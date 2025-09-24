(use-package magit
  :ensure t)

;;
;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93"
     "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3"
     "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7"
     "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f"
     "ba9c91bc43996f2fa710e4b5145d9de231150103e142acdcf24adcaaf0db7a17"
     "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48"
     "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9"
     default))
 '(inhibit-startup-screen t)
 '(ispell-dictionary "british" t)
 '(ispell-program-name "aspell")
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-wheel-mode t)
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-todo-list-sublevels nil)
 '(org-clock-clocked-in-display 'mode-line)
 '(org-clock-mode-line-total 'today)
 '(org-enforce-todo-dependencies t)
 '(org-log-done 'time)
 '(org-startup-indented nil)
 '(package-selected-packages '(eat magit slime))
 '(realgud-safe-mode nil)
 '(tab-width 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Custom functions

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun toggle-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;; Refreshes the file in current buffer.
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  (message "%s" "File refreshed."))

;; Opposite of fill-paragraph.
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((lines) (end (copy-marker end)))
      (goto-char start)
      (while (and (< (point) (marker-position end))
                  (not (eobp)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (if (member line lines)
              (delete-region (point) (progn (forward-line 1) (point)))
            (push line lines)
            (forward-line 1)))))))

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;
;; Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
   (progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;

;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

(defun open-magit-status ()
  (interactive)
  (magit-status))

(defun transpose-words-back (arg)
  (interactive "*p")
  (transpose-subr 'backward-word arg))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (display-buffer (find-file (ffap-guesser)))
  (if (not (equal line-num 0))
      (goto-line line-num))
  (recenter))

(defun frame-adjust ()
  (interactive)
  (if (eq (frame-width) 105)
      (progn
        (set-frame-size (selected-frame) 190 53)
        (split-window-right))
    (progn
      (set-frame-size (selected-frame) 105 53)
      (delete-other-windows))))

(defun reverse-paragraphs (beg end)
  "Reverse the order of paragraphs in a region.
From a program takes two point or marker arguments, BEG and END."
  (interactive "r")
  (when (> beg end)
    (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; the last paragraph might be missing a trailing newline
    (goto-char end)
    (setq end (point-marker))
    ;; the real work.
    (goto-char beg)
    (let (paragraphs fix-newline)
      (while (< beg end)
	;; skip to the beginning of the next paragraph instead of
	;; remaining on the position separating the two paragraphs
	(when (= 0 (forward-paragraph 1))
	  (goto-char (1+ (match-end 0))))
	(when (> (point) end)
	  (goto-char end))
	(setq paragraphs (cons (buffer-substring beg (point))
			       paragraphs))
	(delete-region beg (point)))
      ;; if all but the last paragraph end with two newlines, add a
      ;; newline to the last paragraph
      (when (and (null (delete 2 (mapcar (lambda (s)
					   (when (string-match "\n+$" s -2)
					     (length (match-string 0 s))))
					 (cdr paragraphs))))
		 (when (string-match "\n+$" (car paragraphs) -2)
		   (= 1 (length (match-string 0 (car paragraphs))))))
	(setq fix-newline t)
	(setcar paragraphs (concat (car paragraphs) "\n")))
      ;; insert paragraphs
      (dolist (par paragraphs)
	(insert par))
      (when fix-newline
	(delete-char -1)))))

;;
;; Backups

(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 99999999        ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
(setq vc-make-backup-files t)
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;;
;; Misc settings

(when (display-graphic-p
       (menu-bar-mode -1)))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq auto-window-vscroll nil)
(blink-cursor-mode 0)
(setq frame-title-format "%b")
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(local-set-key (kbd "RET") 'newline-and-indent)
(setq visible-bell nil)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))
(setq transient-mark-mode nil)
(setq user-full-name "Jim Kennedy")
(setq require-final-newline t)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(set-default 'truncate-lines t)
(put 'downcase-region 'disabled nil)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                        64 68 72 76 80 84 88 92 96 100 104 108 112
                        116 120))
(setq-default tab-width 4)
(setq tab-width 4)
(setq ediff-split-window-function 'split-window-horizontally)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq delete-by-moving-to-trash t)
(setq tramp-default-method "ssh")
(set-time-zone-rule "GMT-1")
(fset 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :family "Cascadia Code" :height 120)
(setenv "GIT_ASKPASS" "git-gui--askpass")
;;(global-git-gutter-mode +1)
(put 'erase-buffer 'disabled nil)
(setq mouse-wheel-progressive-speed nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(set-time-zone-rule "GMT")
(setq show-paren-delay 0)
(show-paren-mode)
(setq mac-command-modifier 'control)
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )
(setq ispell-dictionary "british")
(setq css-indent-offset 2)
(setq native-comp-async-report-warnings-errors 'silent)
(setq create-lockfiles nil)
(set-time-zone-rule "Europe/London")

;;
;; Bindings

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key global-map "\C-\M-Q" 'unfill-region)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key [f1] 'ansi-term)
(global-set-key [f5] 'refresh-file)
(global-set-key [f7] 'call-last-kbd-macro)
(global-set-key (kbd "<f12>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)
(global-set-key [f8] 'open-init-file)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c m") 'open-magit-status)
(global-set-key (kbd "C-c g") 'open-magit-status)
(global-set-key (kbd "M-<tab>") 'other-window)
(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)
(global-set-key (kbd "C-.") 'find-file-at-point-with-line)
(global-set-key (kbd "<Scroll_Lock>") 'ignore)
(global-set-key (kbd "C-c C-d") 'redraw-display)
(global-set-key (kbd "C-x m") 'execute-extended-command)

;;
;; Hooks

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Sounds
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;
;; Org mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook
          (function (lambda ()
                      (flyspell-mode)
                      (visual-line-mode)
                      (local-unset-key (kbd "C-c SPC")))))
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h)" "WAIT(w)" "ECHO(e)" "|" "DONE(d)")))

;; Whitespace
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; save the buffer, removing and readding the 'delete-trailing-whitespace function
;; to 'before-save-hook if it's there
(defun save-buffer-no-delete-trailing-whitespace ()
  (interactive)
  (let ((normally-should-delete-trailing-whitespace (memq 'delete-trailing-whitespace before-save-hook)))
    (when normally-should-delete-trailing-whitespace
      (remove-hook 'before-save-hook 'delete-trailing-whitespace))
    (save-buffer)
    (when normally-should-delete-trailing-whitespace
      (add-hook 'before-save-hook 'delete-trailing-whitespace))))

;;
;; Magit

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
(defun disable-magit-highlight-in-buffer ()
  (face-remap-add-relative 'magit-item-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)

;;
;; Local customisations

(setq cust-location "~/.emacs.d/init_local.el")
(when (file-exists-p cust-location)
  (load cust-location))

(defun my-full-screen-buffer-list ()
  "Open buffer list in full screen in the current window."
  (interactive)
  (delete-other-windows)
  (buffer-menu))

(global-set-key (kbd "C-x C-b") 'my-full-screen-buffer-list)

(defun my-magit-switch-project ()
  "Quickly switch between git projects in c:/dev/ with substring matching."
  (interactive)
  (let* ((project-root "c:/dev/")
         (projects (delete "." (delete ".." (directory-files project-root))))
         (completion-styles '(substring flex))
         (completion-ignore-case t)
         (selected (completing-read
                    "Select project: "
                    projects nil t nil
                    'my-magit-switch-project-history)))
    (when selected
      (magit-status (concat project-root selected)))))

(setq savehist-additional-variables
      '(minibuffer-history
        my-magit-switch-project-history))

(global-set-key (kbd "C-c p") 'my-magit-switch-project)
(global-set-key (kbd "C-c C-p") 'my-magit-switch-project)

(savehist-mode 1)

(defun up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
    (delete-minibuffer-contents)))

(define-key minibuffer-local-filename-completion-map
            [C-backspace] #'up-directory)

;;
;; Restart emacs server.

(require 'server)
(or (server-running-p)
    (server-start))

(load-theme 'wombat t)

(setq inferior-lisp-program "sbcl")

(provide '.emacs)
