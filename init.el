
;;
;; Restart emacs server.

(require 'server)
(or (server-running-p)
    (server-start))

;;
;; Common lisp is needed all the time so require that here.

(require 'cl)
(require 'format-spec)

;;
;; Packages

(require 'package)

(setq package-archives '(
                        ("elpa" . "http://tromey.com/elpa/")
                        ("gnu" . "http://elpa.gnu.org/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(avy
    dash
    epc
    exec-path-from-shell
    git-gutter
    helm
    helm-projectile
    helm-git-grep
    jedi
    magit
    multiple-cursors
    paredit
    projectile
    realgud
    undo-tree
    sphinx-doc
    zenburn-theme
    ) "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-initialize)
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
(require 'helm-projectile)

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

(when (display-graphic-p
       (menu-bar-mode -1)))

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


(defun set-M-3-to-hash ()
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#"))))

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
    ("6b2c6e5bc1e89cf7d927d17f436626eac98a04fdab89e080f4e193f6d291c93d" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "ec5f761d75345d1cf96d744c50cf7c928959f075acf3f2631742d5c9fe2153ad" "4ea1959cfaa526b795b45e55f77724df4be982b9cd33da8d701df8cdce5b2955" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default)))
 '(helm-completion-style (quote emacs))
 '(inhibit-startup-screen t)
 '(ispell-dictionary "british")
 '(ispell-program-name "aspell")
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-wheel-mode t)
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-todo-list-sublevels nil)
 '(org-clock-clocked-in-display (quote mode-line))
 '(org-clock-mode-line-total (quote today))
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (swift-mode rainbow-mode helm-git-grep rjsx-mode company omnisharp base16-theme darktooth-theme nord-theme yaml-mode csv-mode markdown-preview-mode csharp-mode js2-mode slime elpy zenburn-theme undo-tree sphinx-doc realgud paredit multiple-cursors magit jedi helm-projectile git-gutter exec-path-from-shell avy)))
 '(realgud-safe-mode nil)
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "SQLALCHEMY_ECHO=1 make run-server")
     (projectile-project-test-cmd . "py.test -svk unit -n 2"))))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indentation-face ((t (:inherit nil))))
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))

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
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'magit-status)
(setq projectile-switch-project-action 'open-magit-status)
(setq projectile-use-git-grep t)
(helm-projectile-on)
(add-to-list 'projectile-globally-ignored-files "*.fasl")

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
(set-face-attribute 'default nil :height 112)
(set-default-font "Cascadia Code")
(setenv "GIT_ASKPASS" "git-gui--askpass")
(global-git-gutter-mode +1)
(put 'erase-buffer 'disabled nil)
(setq mouse-wheel-progressive-speed nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

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
(global-set-key [f1] 'shell)
(global-set-key [f2] 'helm-git-grep-at-point)
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

;;
;; Slime

(setq quicklisp-location "~/quicklisp/slime-helper.el")
(when (file-exists-p quicklisp-location)
  (load (expand-file-name quicklisp-location))
  (setq inferior-lisp-program "sbcl"))

;;
;; Mac OSX

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (set-M-3-to-hash))

;;
;; X11
(when (memq window-system '(x))
  (setq x-alt-keysym 'meta)
  (setq normal-erase-is-backspace t)
  (set-M-3-to-hash))

;; Realgud
(load-library "realgud")

;; Do this last so we have a visual clue initialisation is finished.
(load-theme 'zenburn)




;; Python
(defun insert-py-debug ()
  (interactive)
  (back-to-indentation)
  (insert "console.log();")
  (indent-for-tab-command)
  (backward-char 2))
(global-set-key (kbd "C-'") 'insert-py-debug)

;; Javascript
(setq js-indent-level 2)

;; Whitespace
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Magit
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(defun disable-magit-highlight-in-buffer ()
  (face-remap-add-relative 'magit-item-highlight '()))

(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)

;; Omnisharp
(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;; Css
(setq css-indent-offset 2)

;; (require 'mouse)
;; (xterm-mouse-mode t)
;; (mouse-wheel-mode t)

;; (global-set-key (kbd "<wheel-up>") '(lambda () (interactive) (scroll-up 1)))
;; (global-set-key (kbd "<wheel-down>") '(lambda () (interactive) (scroll-down 1)))
;; (define-key key-translation-map "\033[M'X1-" (kbd "<wheel-up>"))

;;
;; Local customisations
(setq cust-location "~/.emacs.d/init_local.el")
(when (file-exists-p cust-location)
  (load cust-location))

(set-time-zone-rule "GMT")

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("screens\\/.*\\.js\\'" . rjsx-mode))

(provide '.emacs)
;;; .emacs ends here
