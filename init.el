
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; Packages

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package avy
  :ensure t
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package define-word
  :ensure t)

(use-package elfeed
  :ensure t)

(use-package ein
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Documents/elfeed.org")))

(use-package engine-mode
  :ensure t
  :config
  (setq engine/browser-function 'browse-url-firefox)
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d"))

(use-package ess
  :ensure t
  :config
  (ess-toggle-underscore nil))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-translation-directions-alist
	'(("en" . "ja") ("ja" . "en") ("en" . "zh-CN") ("ja" . "zh-CN") ("en" . "zh-TW") ("ja" . "zh-TW")))
  (setq google-translate-show-phonetic t))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-launcher (:color blue)
    ("dw" define-word-at-point "define-word-at-point")
    ("de" define-word "define-word")
    ("gi" engine/search-github "Github")
    ("du" engine/search-duckduckgo "DuckDuckGo")
    ("gt" google-translate-smooth-translate "google-translate-smooth-translate")
    ("z" zeal-at-point "zeal-at-point")
    ("q" nil))
  (global-set-key (kbd "C-c k") 'hydra-launcher/body)

  (add-hook 'org-mode-hook 'org-indent-mode)

  (require 'org-agenda)

  (defhydra hydra-org-agenda-clock (:color blue :hint nil)
    ("i" org-agenda-clock-in)
    ("o" org-agenda-clock-out)
    ("q" org-agenda-clock-cancel)
    ("g" org-agenda-clock-goto))

  (bind-keys ("C-c w" . hydra-org-clock/body)
             :map org-agenda-mode-map
             ("C-c w" . hydra-org-agenda-clock/body))

  (defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))

  (defhydra hydra-zoom ()
    "zoom"
    ("+" text-scale-increase "in")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("_" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color blue))

  (bind-keys ("C-x C-0" . hydra-zoom/body)
	     ("C-x C-=" . hydra-zoom/body)
	     ("C-x C--" . hydra-zoom/body)
	     ("C-x C-+" . hydra-zoom/body))


  (defhydra hydra-outline (:color pink :hint nil)
  "
  ^Hide^             ^Show^           ^Move
  ^^^^^^------------------------------------------------------
  _q_: sublevels     _a_: all         _u_: up
  _t_: body          _e_: entry       _n_: next visible
  _o_: other         _i_: children    _p_: previous visible
  _c_: entry         _k_: branches    _f_: forward same level
  _l_: leaves        _s_: subtree     _b_: backward same level
  _d_: subtree
  "
  ;; Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave"))

  (global-set-key (kbd "C-c #") 'hydra-outline/body)


  (defhydra hydra-dired (:hint nil :color pink)
  "
  _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired            :d decrypt
  _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit    :e encrypt
  _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit  :s sign
  _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort   :v verify
  _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
  _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
  _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
  _z_ compress-file  _A_ find regexp
  _Z_ compress       _Q_ repl regexp
  T - tag prefix
  "
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

  (eval-after-load "dired" '(progn
    (define-key dired-mode-map (kbd "M-o") 'other-window) )))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (global-set-key "\C-s" 'swiper))
  
(use-package counsel
  :ensure t)
  
(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package ledger-mode
  :ensure t)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package migemo
  :ensure t
  :config

  (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; Set your installed path
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))
  
(use-package avy-migemo
  :ensure t
  :config
  (avy-migemo-mode 1)
  (require 'avy-migemo-e.g.swiper))
  
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package org-board
  :ensure t
  :config
  (global-set-key (kbd "C-c o") org-board-keymap))

(use-package org-bullets
  :ensure t
  :config
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package pass
  :ensure t)

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode)))

(use-package sudo-edit
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package zeal-at-point
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(setq org-agenda-files	
      '("~/Documents/Notes/memacs/photos.org_archive"
	"~/Documents/Notes/memacs/git-dotfiles.org_archive"
	"~/Documents/Notes/memacs/git-emacs.org_archive"))
  
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
  (defun place-agenda-tags ()
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))
  
(add-hook 'org-agenda-mode-hook #'hl-line-mode 'append)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("n" "note" entry (file+datetree "~/org/notes.org")
	 "* %?\n")
	("a" "article" entry (file+headline "~/org/organizer.org" "Read")
	 "* TODO %?\n  :PROPERTIES:\n  :URL: \n:END:\n  Captured on %U\n")
	("p" "programming project idea" entry (file+headline "~/org/programming.org" "Project Ideas")
	 "* %?\n")
	("j" "journal" entry (file+datetree "~/org/journal.org")
	 "* %?\n  Entered on %U\n")
	("t" "task" entry (file+headline "~/org/organizer.org" "Inbox")
	 "* TODO %?\n")
	("k" "今日よかったこと" entry (file+datetree "~/org/journal.org")
	 "* 今日よかったこと\n** %?\n** \n** \n  Entered on %U\n")
	("u" "quote" entry (file+datetree "~/org/notes.org")
	 "* %? :quote:\n Source: ")
	("r" "daily routine" entry (file+headline "~/org/routines.org" "Daily")
	 "* TODO %?\n")))

;; http://orgmode.org/worg/org-contrib/
(require 'org-checklist)

;; org-habits https://orgmode.org/manual/Tracking-your-habits.html
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60
      org-habit-preceding-days 14
      org-habit-following-days 1
      org-habit-show-habits-only-for-today t)

(setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 6)))

(setq org-startup-truncated nil)

;; reload buffer when file has been updated
(global-auto-revert-mode t)

;;; backups

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq vc-make-backup-files t) ;; Backup versioned files

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each emacs session.
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

(setq inhibit-startup-screen t
initial-scratch-message nil)

(require 'epa-file)
(epa-file-enable)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(display-time-mode 1)
(setq display-time-format "%Y/%m/%d %H:%M")
(setq display-time-default-load-average nil)

(require 'mozc)  ; or (load-file "/path/to/mozc.el")
(setq default-input-method "japanese-mozc")
(setq mozc-candidate-style 'echo-area)

(add-to-list 'default-frame-alist '(font . "Noto Sans Mono CJK JP Regular-12"))

(global-set-key (kbd "C-c l") 'org-store-link)
