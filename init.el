
;;
;; Restart emacs server.

(require 'server)
(or (server-running-p)
    (server-start))

;;
;; Common lisp is needed all the time so require that here.

(require 'cl)

;;
;; Packages

(require 'package)
(package-initialize)

;; Set up extra package sources.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Guarantee all packages are installed on start
(defvar packages-list
  '(auto-complete              ;; Auto Completion for GNU Emacs
    epc                        ;; A RPC stack for the Emacs Lisp
    fullscreen-mode            ;; fullscreen window support for Emacs
    helm                       ;; Helm is an Emacs incremental and narrowing framework
    helm-projectile            ;; Helm integration for Projectile
    jedi                       ;; Python auto-completion for Emacs
    magit                      ;; control Git from Emacs
    multiple-cursors           ;; Multiple cursors for Emacs.
    paredit                    ;; minor mode for editing parentheses
    projectile                 ;; Manage and navigate projects in Emacs easily
    ) "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;;
;; Require.

(require 'helm)
(require 'helm-config)
(require 'ispell)

;;
;; Custom functions

(defun execute-commands (buffer &rest commands)
  "Execute a list of shell commands sequentially"
  (with-current-buffer buffer
    (set (make-local-variable 'commands-list) commands)
    (start-next-command)))

(defun start-next-command ()
  "Run the first command in the list"
  (if (null commands-list)
      (insert "\nDone.")
    (let ((command  (car commands-list)))
      (setq commands-list (cdr commands-list))
      (insert (format ">>> %s\n" command))
      (let ((process (start-process-shell-command command (current-buffer) command)))
        (set-process-sentinel process 'sentinel)))))

(defun sentinel (p e)
  "After a process exited, call `start-next-command' again"
  (let ((buffer (process-buffer p)))
    (when (not (null buffer))
      (with-current-buffer buffer
                                        ;(insert (format "Command `%s' %s" p e) )
        (start-next-command)))))

(defun describe-last-function()
  (interactive)
  (describe-function last-command))

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

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

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

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;;
;; Custom set variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default)))
 '(drupal-convert-line-ending nil)
 '(drupal-delete-trailing-whitespace (quote never))
 '(geben-pause-at-entry-line t)
 '(inhibit-startup-screen t)
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-wheel-mode t)
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-todo-list-sublevels nil)
 '(org-clock-clocked-in-display (quote mode-line))
 '(org-clock-mode-line-total (quote today))
 '(org-enforce-todo-dependencies t)
 '(org-startup-indented t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
;; Projectile

(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)

;;
;; Helm

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-autoresize-mode t)
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

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

;;
;; Settings

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
(setq browse-url-browser-function 'browse-url-default-windows-browser)
(setq visible-bell t)
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

;;
;; Bindings

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key global-map "\C-\M-Q" 'unfill-region)
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "C-'")  'insert-php-debug)
(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)
(global-set-key [f1] 'shell)
(global-set-key [f5] 'refresh-file)
(global-set-key [f7] 'call-last-kbd-macro)
(global-set-key (kbd "<f12>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key [f8] 'dired-time)
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

;;
;; Slime

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;
;; Do this last so we have a visual clue initialisation is finished.

(load-theme 'wombat)

(provide '.emacs)
;;; .emacs ends here
