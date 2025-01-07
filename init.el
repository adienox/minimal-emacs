;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

(defconst nox/emacs-directory (concat (getenv "HOME") "/.config/aurora-emacs/" ))
(defconst nox/notes-directory (concat (getenv "HOME") "/Documents/Zettels/"))

;;; init.el --- file for init -*- no-byte-compile: t; lexical-binding: t; -*-

(defun nox/display-startup-time ()
  "Display the emacs startup time"
  (interactive)
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(defconst nox/emacs-directory (concat (getenv "HOME") "/.config/minimal-emacs/" ))
(defconst nox/notes-directory (concat (getenv "HOME") "/Documents/Zettels/"))

(add-to-list 'load-path (concat nox/emacs-directory "scripts/"))

;; setting the core date by searching from hydra for nixos
(setq elpaca-core-date '(20241224))
(require 'elpaca-setup)   ;; Elpaca Package Manager
(require 'on)             ;; Doom Style Hooks
(require 'minimal)        ;; emacs config from minimal-emacs.d
(require 'link-converter) ;; md <=> converter

;; Specify the custom file path
(setq custom-file (concat nox/emacs-directory "custom-vars.el"))

;; Load the custom file quietly
(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file 'noerror 'nomessage)))

(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-fu)
  (evil-vsplit-window-right t)
  (evil-vsplit-window-below t)
  :config
  (evil-mode)

(evil-define-key 'motion 'global
  (kbd "C-w u") 'winner-undo
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(evil-define-key 'normal 'global
  (kbd "C-S-v") 'cua-set-mark
  (kbd "C-o") 'casual-editkit-main-tmenu
  "s" 'evil-avy-goto-char-timer)

(evil-define-key '(normal visual) 'global
  "P" 'consult-yank-from-kill-ring
  "H" 'evil-first-non-blank
  "L" 'evil-end-of-line)

(evil-define-key 'normal org-mode-map
  "J" 'org-shiftright
  "K" 'org-shiftleft)

(evil-define-key 'normal elfeed-search-mode-map
  "l" 'elfeed-search-show-entry))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :custom
  (evil-collection-want-find-usages-bindings t))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer nox/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

(nox/leader-keys
  "a" '(:ignore t :wk "[A]pplications")
  "a e" '(elfeed :wk "[E]lfeed")
  "a g" '(gptel :wk "[G]ptel")
  "a m" '(mu4e :wk "[M]ail"))

(nox/leader-keys
  "b" '(:ignore t :wk "[B]uffer")
  "b b" '(consult-buffer :wk "[B]uffer Switch")
  "b i" '(persp-ibuffer :wk "[I]buffer")
  "b k" '(kill-current-buffer :wk "[K]ill Buffer")
  "b n" '(next-buffer :wk "[N]ext Buffer")
  "b p" '(previous-buffer :wk "[P]revious Buffer")
  "b s" '(hydra-buffer-switch/body :wk "[S]witch Buffer")
  "b r" '(revert-buffer :wk "[R]eload Buffer"))

(nox/leader-keys
  "d" '(:ignore t :wk "[D]ired")
  "d ." '(dired-omit-mode :wk "Toggle dot files")
  "d d" '(dired-jump :wk "[D]ired")
  "d h" '(dired-hide-details-mode :wk "[D]ired")
  "d p" '(peep-dired :wk "[P]eep Dired"))

(nox/leader-keys
  "e" '(:ignore t :wk "[E]val")
  "e b" '(eval-buffer :wk "[B]uffer Eval")
  "e d" '(eval-defun :wk "[D]efun Eval")
  "e e" '(eval-expression :wk "[E]xpression Eval")
  "e l" '(eval-last-sexp :wk "[E]xpression Before Eval")
  "e r" '(eval-region :wk "[R]egion Eval"))

(nox/leader-keys
  "f" '(:ignore t :wk "[F]ile")
  "f c" `((lambda () (interactive) (find-file ,(concat nox/emacs-directory "config.org")))
          :wk "Edit emacs config")
  "f s" '(save-buffer :wk "[S]ave Buffer")
  "f u" '(sudo-edit-find-file :wk "S[U]do Find File")
  "f U" '(sudo-edit :wk "S[U]do Edit File"))

(nox/leader-keys
  "g" '(:ignore t :wk "[G]it")
  "g g" '(magit-status :wk "[G]it Status")
  "g s" '(diff-hl-stage-dwim :wk "[G]it Stage Hunk"))

(nox/leader-keys
  "o" '(:ignore t :wk "[O]rg")
  "o a" '(org-agenda :wk "[A]genda")
  "o e" '(org-edit-src-code :wk "[E]dit Src Code")
  "o x" '(org-toggle-checkbox :wk "[C]heckbox")
  "o l" '(org-store-link :wk "[L]ink Store")
  "o b" '(:ignore t :wk "[B]abel")
  "o b t" '(org-babel-tangle :wk "[T]angle")
  "o b d" '(org-babel-demarcate-block :wk "[D]emarcate Block"))

(nox/leader-keys
  "o p" '(:ignore t :wk "[P]roperties")
  "o p s" '(org-schedule :wk "[S]chedule")
  "o p d" '(org-deadline :wk "[D]eadline")
  "o p e" '(org-set-effort :wk "[E]ffort")
  "o p t" '(nox/org-toggle-properties :wk "[T]oggle Properties")
  "o p p" '(org-set-effort :wk "[P]roperty"))

(nox/leader-keys
  "o i" '(org-roam-node-insert :wk "[I]nsert Link")
  "o c" '((lambda () (interactive) (org-roam-capture nil "d")) :wk "[C]apture")
  "o C" '(org-roam-capture :wk "[C]apture with Templates")
  "o f" '(org-roam-node-find :wk "[F]ind Node")
  "o t" '(nox/org-roam-capture-tasks :wk "[T]ask Capture")
  "o d" '((lambda () (interactive) (org-roam-dailies-capture-today nil "d")) :wk "[D]aily Capture")
  "o D" '(org-roam-dailies-capture-today :wk "[D]aily Open"))

(nox/leader-keys
  "q" '(:ignore t :wk "[Q]uit")
  "q f" '(delete-frame :wk "[F]rame delete")
  "q r" '(nox/restore-perspectives :wk "[R]estore perspectives")
  "q K" '(kill-emacs :wk "[K]ill emacs"))

(nox/leader-keys
  "s" '(:ignore t :wk "[S]earch")
  "s g" '(consult-ripgrep :wk "[G]rep in dir")
  "s i" '(consult-imenu :wk "[I]menu")
  "s f" '(consult-fd :wk "[F]d Consult")
  "s r" '(consult-recent-file :wk "[R]recent File")
  "s m" '(bookmark-jump :wk "[M]arks")
  "s c" '(consult-mode-command :wk "[C]ommands for mode"))

(nox/leader-keys
  "t" '(:ignore t :wk "[T]oggle")
  "t e" '(eshell-toggle :wk "[E]shell")
  "t l" '(elpaca-log :wk "[L]og Elpaca")
  "t t" '(modus-themes-toggle :wk "[T]oggle Theme")
  "t c" '(visual-fill-column-mode :wk "[C]olumn Fill Mode")
  "t v" '(vterm-toggle :wk "[V]term")
  "t n" '(display-line-numbers-mode :wk "[N]umbered Lines")
  "t s" '(hydra-text-scale/body :wk "[S]cale Text"))

(nox/leader-keys
  "." '(find-file :wk "Find File")
))

(use-package all-the-icons)
(use-package nerd-icons :defer 2)

(use-package doom-modeline
  :hook
  (on-init-ui . doom-modeline-mode)
  :config
  (setq find-file-visit-truename t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-height 36))

(use-package hide-mode-line :commands hide-mode-line-mode)

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode)
  :init
  (electric-indent-mode -1)    ;; Disable weird emacs indenting
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation
  (line-number-mode -1)        ;; Disable line number from showing in modline
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.
  (electric-pair-mode 1)       ;; Enable pair parens
  (display-battery-mode 1)     ;; Enable displaying battery info in modline
  (winner-mode 1)              ;; Easily undo window configuration changes.
  :custom
  (delete-selection-mode 1)             ;; Replacing selected text with typed text.
  (global-visual-line-mode 1)           ;; Better text wrapping
  (display-line-numbers-type 'relative) ;; Use relative line numbering
  (history-length 25)                   ;; Set the length of the command history.
  (ispell-dictionary "en_US")           ;; Default dictionary for spell checking.
  (pixel-scroll-precision-mode t)       ;; Enable precise pixel scrolling.
  (ring-bell-function 'ignore)          ;; Disable the audible bell.
  (tab-width 4)                         ;; Set the tab width to 4 spaces.
  (use-dialog-box nil)                  ;; Disable dialog boxes
  (warning-minimum-level :error)        ;; Set the minimum level of warnings.
  (show-paren-context-when-offscreen t) ;; Show context of parens when offscreen
  (pixel-scroll-precision-use-momentum nil) ;; Disable momentum scrolling

  ;; Don't jump the cursor around when scrolling
  (scroll-conservatively 101)
  ;; TAB key complete, instead of just indenting.
  (tab-always-indent 'complete)
  ;; Use advanced font locking for Treesit mode.
  (treesit-font-lock-level 4)
  ;; Offer to delete any autosave file when killing a buffer.
  (kill-buffer-delete-auto-save-files t)
  ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (split-width-threshold 300)
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq-default indent-tabs-mode nil))

(use-package woman
  :ensure nil
  :hook
  (woman-mode . visual-fill-column-mode)
  :custom
  (woman-fill-frame t))

(setq auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 20    ; number of seconds idle time before auto-save
      auto-save-interval 200  ; number of keystrokes between auto-saves

      ;; Do not auto-disable auto-save after deleting large chunks of text
      auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

(global-auto-revert-mode 1)  ;; Keep buffers up to date with their files.
(setq global-auto-revert-non-file-buffers t)   ;; Automatically refresh non-file buffers.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; setting the backup dir to trash.

(setq backup-directory-alist '(("." .
                                  (concat (getenv "XDG_DATA_HOME") "/Trash/files")))
        make-backup-files t     ; backup of a file the first time it is saved.
        backup-by-copying t     ; don't clobber symlinks
        version-control t       ; version numbers for backup files
        delete-old-versions t   ; delete excess backup files silently
        kept-old-versions 6     ; oldest versions to keep when a new numbered
        kept-new-versions 9)    ; newest versions to keep when a new numbered

(save-place-mode 1)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

(setq savehist-additional-variables
      '(kill-ring
        command-history
        set-variable-value-history
        custom-variable-history
        query-replace-history
        read-expression-history
        minibuffer-history
        read-char-history
        face-name-history
        bookmark-history
        file-name-history))

(setq history-length 300)
(setq kill-ring-max 25)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(savehist-mode 1)

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(setq recentf-max-menu-items 25)
(recentf-mode 1)
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-auto-cleanup 'mode)

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `italic', `WEIGHT'
        modus-themes-prompts '(italic bold)

        ;; The `modus-themes-completions' is an alist that reads two
        ;; keys: `matches', `selection'.  Each accepts a nil value (or
        ;; empty list) or a list of properties that can include any of
        ;; the following (for WEIGHT read further below):
        ;;
        ;; `matches'   :: `underline', `italic', `WEIGHT'
        ;; `selection' :: `underline', `italic', `WEIGHT'
        modus-themes-completions
        '((matches . (extrabold))
          (selection . (semibold italic text-also)))

        ;; Remove the border
        ;; Make the fringe invisible
        modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified))

        modus-themes-to-toggle
        '(modus-operandi-tinted modus-vivendi-tinted)

        modus-themes-headings
        '((1 . (variable-pitch 1.5))
          (2 . (1.3))
          (3 . (1.1))
          (agenda-date . (1.3))
          (agenda-structure . (variable-pitch light 1.8))
          (t . (1.1))))

  (load-theme 'modus-vivendi-tinted t))

;; adding padding to ui elements to make doing tasks feel more comfortable
(use-package spacious-padding
  :after modus-themes
  :config
  (spacious-padding-mode))

(use-package which-key
  :config (which-key-mode)
  :ensure nil
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 10)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.3)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)
  (which-key-separator " → " ))

(use-package avy
  :commands evil-avy-goto-char-timer
  :custom
  (avy-background t)
  (avy-timeout-seconds 0.5)
  :config
  (defun nox/avy-jump-org-block ()
    "Jump to org block using Avy subsystem."
    (interactive)
    (avy-jump (rx line-start (zero-or-more blank) "#+begin_src")
              :action 'goto-char)
    ;; Jump _into_ the block:
    (forward-line))
  (defun nox/avy-jump-to-link ()
    "Jump to org block using Avy subsystem."
    (interactive)
    (avy-jump (rx (or "http://" "https://")) :action 'goto-char))
  (set-face-attribute 'avy-background-face nil
                      :background 'unspecified))

(defun nox/browse-url-maybe-privately (url &optional new-window)
  "Ask whether URL should be browsed in a private browsing window."
  (interactive "URL: ")
  (if (y-or-n-p "Private Browsing? ")
      (nox/browse-url-firefox-privately url)
    (browse-url-default-browser url new-window)))

(defun nox/browse-url-firefox-privately (url &optional new-window)
  "Make firefox open URL in private-browsing window."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           browse-url-firefox-program
           (list "-private-window" url))))

(defun nox/open-in-reddigg (url &optional new-window)
  "Open the provided url in reddigg"
  (reddigg-view-comments url))

(defun nox/parse-readwise (url &optional new-window)
  "Extract, decode and open the save URL part from a given Readwise URL."
  (if (string-match "https://wise\\.readwise\\.io/save\\?url=\\(.*\\)" url)
      (nox/browse-url-maybe-privately (url-unhex-string (match-string 1 url)))
    (error "Invalid URL format")))

(setq browse-url-handlers
      '(("^https?://www\\.reddit\\.com" . nox/open-in-reddigg)
        ("^https?://arstechnica\\.com" . eww)
        ("^https?://wise\\.readwise\\.io/save\\?url=" . nox/parse-readwise)
        ("." . nox/browse-url-maybe-privately)))

(setq browse-url-generic-program "firefox")

(setq display-buffer-alist
      '(("\\*Occur\\*"
         (display-buffer-reuse-mode-window)
         (display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))
        ))

(defun nox/run-commands-for-buffer-names ()
  "Run specific commands for certain buffer names."
  (let ((buffer-name (buffer-name)))
    (cond
     ((string-prefix-p "*ChatGPT" buffer-name)
      ;; make the window dedicated
      (set-window-dedicated-p (selected-window) t))

     ((string= buffer-name "*use-package statistics*")
      (hl-line-mode))

     ((string= buffer-name "*reddigg-comments*")
      (org-appear-mode -1)
      (evil-goto-first-line)
      ;; convert all md links to org links
      (nox/md-to-org-links)
      ;; make the window dedicated
      (set-window-dedicated-p (selected-window) t)
      ;; easier quitting of the window
      (evil-local-set-key 'normal "q" 'kill-current-buffer)
      ;; open all folds
      (org-fold-show-all)
      (read-only-mode))
     )))

;; Add the function to hooks
(add-hook 'buffer-list-update-hook 'nox/run-commands-for-buffer-names)

(use-package gptel
  :commands gptel
  :hook
  (gptel-mode . evil-insert-state)
  (gptel-post-stream . gptel-auto-scroll)
  (gptel-post-response-functions . gptel-end-of-response)
  :bind* (("C-c RET" . gptel-send))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-api-key
   (nth 0 (process-lines "cat"
                         (concat
                          (getenv "HOME")
                          "/.config/sops-nix/secrets/api_keys/openai"))))
  :config
  (gptel-make-kagi "Kagi"
    :key
    (nth 0 (process-lines "cat"
                          (concat
                           (getenv "HOME")
                           "/.config/sops-nix/secrets/api_keys/kagi"))))
  (gptel-make-gemini "Gemini"
    :key
    (nth 0 (process-lines "cat"
                          (concat
                           (getenv "HOME")
                           "/.config/sops-nix/secrets/api_keys/gemini")))
    :stream t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package elfeed
  :commands elfeed
  :custom
  (elfeed-search-filter "@1-weeks-ago +unread")
  :config
  (require 'nano-elfeed)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "j") 'nano-elfeed-next-entry
    (kbd "k") 'nano-elfeed-prev-entry)
  (defun nox/elfeed-show (buff)
    (switch-to-buffer buff)
    (display-line-numbers-mode -1)
    (visual-fill-column-mode)
    (elfeed-show-refresh))
  (setq elfeed-show-entry-switch 'nox/elfeed-show))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files `(,(concat nox/emacs-directory "elfeed.org")))
  :config (elfeed-org))

(use-package elfeed-tube
  :after elfeed
  :custom
  (elfeed-tube-auto-save-p t)
  (elfeed-tube-fields '(duration thumbnail captions chapters))
  :config
  (elfeed-tube-setup)
  :general
  (:keymaps 'elfeed-show-mode-map :states 'normal
            "F" 'elfeed-tube-mpv-follow-mode
            "M" 'elfeed-tube-mpv
            "M-RET" (lambda () (interactive) (nox/avy-jump-to-link) (browse-url-generic))
            [remap save-buffer] 'elfeed-tube-save)
  (:keymaps 'elfeed-search-mode-map :states 'normal
            "F" 'elfeed-tube-fetch
            [remap elfeed-search-fetch] 'elfeed-update
            [remap save-buffer] 'elfeed-tube-save))

(use-package elfeed-tube-mpv :after efleed-tube)

(use-package reddigg :commands reddigg-view-comments)

(add-hook 'eww-mode-hook (lambda ()
                           (display-line-numbers-mode -1)
                           (visual-fill-column-mode)))

(use-package mu4e
  :ensure nil
  :defer 5
  :hook
  (mu4e-main-mode . visual-fill-column-mode)
  (mu4e-view-mode . visual-fill-column-mode)
  :config
  (mu4e t)
  (setq user-mail-address "adwait@adhk.dev")
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 5 minutes
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail/proton")

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-refile-folder "/Archive")
  (setq mu4e-trash-folder  "/Trash")

  ;; prefer text/plain when viewing mail
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext" t))

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Archive"                :key ?a)
          (:maildir "/Drafts"                 :key ?d)
          (:maildir "/Inbox"                  :key ?i)
          (:maildir "/Sent"                   :key ?s)
          (:maildir "/Folders/Wisdom Letters" :key ?w))))

(use-package pdf-tools
  :ensure nil
  :hook
  (pdf-view-mode . (lambda ()
                     (pdf-view-themed-minor-mode)
                     (set (make-local-variable 'evil-normal-state-cursor) (list nil))))
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :config
  (package-initialize)
  (pdf-tools-install)
  (add-to-list 'revert-without-query ".pdf"))

(use-package org-pdftools
   :ensure nil
   :hook (org-mode . org-pdftools-setup-link))

(use-package uniline :commands uniline-mode)

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ibuffer
  :ensure nil
  :commands ibuffer
  :hook
  (ibuffer-mode . (lambda () (display-line-numbers-mode -1)))
  (ibuffer-mode . (lambda () (visual-line-mode -1))))

(use-package perspective
  :commands (nox/restore-perspectives persp-state-load)
  :custom
  (persp-state-default-file "~/.local/share/persp-state")
  (persp-mode-prefix-key (kbd "C-c b"))
  (persp-modestring-short t)
  (persp-initial-frame-name "1 aurora")
  (persp-modestring-dividers '("" "" ""))
  :config
  (add-hook 'kill-emacs-hook #'persp-state-save))

(defun nox/restore-perspectives ()
  "Restores the last saved perspective-state and deletes all other frames"
  (interactive)
  (persp-state-load persp-state-default-file)
  (delete-other-frames))

(with-eval-after-load 'evil
  (evil-define-key '(normal insert) 'global
    (kbd "C-S-h") '(lambda () (interactive) (persp-switch-by-number 1))
    (kbd "C-S-j") '(lambda () (interactive) (persp-switch-by-number 2))
    (kbd "C-S-k") '(lambda () (interactive) (persp-switch-by-number 3))
    (kbd "C-S-l") '(lambda () (interactive) (persp-switch-by-number 4))))

(use-package corfu
  :hook (on-first-input . global-corfu-mode)
  :custom
  (corfu-cycle t)                 ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                  ;; Enable auto completion
  (corfu-auto-prefix 2)           ;; Enable auto completion
  (corfu-auto-delay 0.24)         ;; Enable auto completion
  (corfu-preview-current 'insert) ;; Disable current candidate preview
  (corfu-on-exact-match nil)      ;; Configure handling of exact matches
  (corfu-scroll-margin 5)         ;; Use scroll margin
  (corfu-quit-at-boundary 'separator)
  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator))
  :config
  (add-to-list 'corfu--frame-parameters '(alpha-background . 0.9))

  (add-to-list
   'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))

  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :defer 5
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-history
  :after (corfu savehist)
  :ensure nil
  :hook
  (corfu-mode . corfu-history-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure nil
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :config
  ;; popup info delay
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

(use-package emacs
  :ensure nil
  :custom
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package apheleia
  :hook
  (prog-mode . apheleia-mode)
  :config
  (push '(nix-ts-mode . nixfmt) apheleia-mode-alist))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
	 (note "»" compilation-info))))

(use-package lsp-mode
  :commands lsp
  :hook
  (bash-ts-mode . lsp-deferred)                  ;; Enable LSP for Bash
  (go-ts-mode . lsp-deferred)                    ;; Enable LSP for Go
  (lsp-mode . lsp-enable-which-key-integration)  ;; Integrate with Which Key
  :custom
  (lsp-keymap-prefix "C-c l")                    ;; Set the prefix for LSP commands
  (lsp-inlay-hint-enable t)                      ;; Enable inlay hints
  (lsp-completion-provider :none)                ;; Disable default completion provider
  (lsp-log-io nil)                               ;; Disable IO logging for speed
  (lsp-idle-delay 0)                             ;; Set the delay for LSP to 0
  (lsp-keep-workspace-alive nil)                 ;; Disable keeping workspace alive
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Session file location

  ;; Core settings
  (lsp-enable-xref t)                            ;; Enable cross-references.
  (lsp-auto-configure t)                         ;; Automatically configure LSP.
  (lsp-enable-links nil)                         ;; Disable links.
  (lsp-eldoc-enable-hover t)                     ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                 ;; Disable file watchers.
  (lsp-enable-folding nil)                       ;; Disable folding.
  (lsp-enable-imenu t)                           ;; Enable Imenu support.
  (lsp-enable-indentation nil)                   ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)            ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)         ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)             ;; Enable symbol highlighting.
  (lsp-enable-text-document-color nil)           ;; Disable text document color.

  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)         ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)          ;; Use `flymake' instead.
  (lsp-modeline-workspace-status-enable t)       ;; Display "LSP" in the modeline
  (lsp-signature-doc-lines 1)                    ;; Limit echo area to one line.
  (lsp-eldoc-render-all nil)                     ;; Render only signature messages.

  ;; Completion settings
  (lsp-completion-enable t)                      ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t) ;; Enable additional text edits.
  (lsp-enable-snippet nil)                       ;; Disable snippets
  (lsp-completion-show-kind t)                   ;; Show kind in completions.
  (lsp-completion-show-detail nil)               ;; Disable long signature details

  ;; Lens settings
  (lsp-lens-enable t)                            ;; Enable lens support.

  ;; Headerline settings
  (lsp-headerline-breadcrumb-icons-enable t)            ;; Enable icons
  (lsp-headerline-breadcrumb-enable-diagnostics nil)    ;; Disable diagnostics
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil) ;; Disable numbers

  ;; Disable semantic tokens.
  (lsp-semantic-tokens-enable nil)

  ;; nix
  (lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }")
  (lsp-nix-nixd-nixos-options-expr
   "(builtins.getFlake \"/home/nox/aurora/flakes\").nixosConfigurations.anomaly.options")
  (lsp-nix-nixd-home-manager-options-expr
   "(builtins.getFlake \"/home/nox/aurora/flakes\").homeConfigurations.nox.options")
  )

(with-eval-after-load 'lsp-mode
  ;; need lsp-treemacs to make icons in breadcrumb to work
  (use-package lsp-treemacs :after lsp-mode)
  ;; lsp theming
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :underline t
                      :inherit nil))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-border "#585b70")
  (lsp-ui-doc-enable t))

(use-package nix-mode :mode "\\.nix\\'")
(use-package nushell-mode :mode "\\.nu\\'")

(use-package nix-ts-mode :hook (nix-ts-mode . lsp-deferred))
(use-package nushell-ts-mode :hook (nushell-ts-mode . lsp-deferred))

(use-package ligature
  :hook (on-first-input . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode org-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "\\\\" "://")))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

;; (use-package sops
;;   :mode "\\.sops.yaml\\'"
;;   :hook
;;   (sops-mode . yaml-ts-mode)
;;   :bind (("C-c C-c" . sops-save-file)
;;          ("C-c C-k" . sops-cancel)
;;          ("C-c C-d" . sops-edit-file))
;;   :config
;;   (global-sops-mode))

(use-package treesit-auto
  :hook
  (on-first-file . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package dired
  :after evil
  :ensure nil
  :commands (dired dired-jump)
  :hook
  (dired-mode . hl-line-mode)
  ;; To hide dot-files by default
  (dired-mode . dired-omit-mode)
  ;; removing line numbers
  (dired-mode . (lambda () (display-line-numbers-mode -1)))
  (image-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  ;; hide files/directories starting with "." in dired-omit-mode
  (dired-omit-files (rx (seq bol ".")))
  ;; Display files in a human-readable format and group directories first
  ;; Also remove owner and group information
  (dired-listing-switches "-agho --group-directories-first")

  ;; Enable "do what I mean" for target directories
  (dired-dwim-target t)

  ;; Close the previous buffer when opening a new `dired' instance
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (setq dired-free-space nil
        dired-deletion-confirmer 'y-or-n-p
        dired-filter-verbose nil
        dired-clean-confirm-killing-deleted-buffers nil
        dired-recursive-deletes 'top
        dired-recursive-copies  'always
        dired-create-destination-dirs 'ask)
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-open-file))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :commands (dired dired-jump))

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :commands (dired dired-jump))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions '(("gif" . "imv")
                                ("jpg" . "imv")
                                ("webp" . "imv")
                                ("png" . "imv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package dired-rainbow
  :commands (dired dired-jump)
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules")))

(use-package peep-dired
  :commands (dired dired-jump)
  :config
  (evil-define-key 'normal peep-dired-mode-map
    (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file))

(set-face-attribute 'variable-pitch nil
                    :family "Inter"
                    :height 140
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :family "CaskaydiaCove Nerd Font"
                    :height 140
                    :weight 'medium)
(set-face-attribute 'default nil :inherit 'fixed-pitch)
(set-face-attribute 'fixed-pitch-serif nil :inherit 'fixed-pitch :family 'unspecified)

;; setting the emoji font family
;; https://emacs.stackexchange.com/a/80186
(set-fontset-font t 'emoji
                  '("Apple Color Emoji" . "iso10646-1") nil 'prepend)


;; italic comments and keywords
(set-face-attribute 'font-lock-comment-face nil
                    :italic t)

;; setting the line spacing
(setq-default line-spacing 0.16)

(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-14"))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package diff-hl
  :hook (vc-dir-mode . diff-hl-mode)
  :hook (lsp-mode . diff-hl-mode)
  :commands
  (diff-hl-stage-current-hunk
   diff-hl-revert-hunk
   diff-hl-next-hunk
   diff-hl-previous-hunk)
  :custom
  ;; Set the side for diff indicators.
  (diff-hl-side 'left)
  ;; Customize symbols for each change type.
  (diff-hl-margin-symbols-alist '((insert . "│")
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i")))
  (diff-hl-show-staged-changes nil)
  :config
  ;; implements highlighting changes on the fly.
  (diff-hl-flydiff-mode 1)
  ;; changes the highlighting function to use the margin instead of the fringe.
  (diff-hl-margin-mode 1))

(use-package transient)
(use-package magit
  :commands magit-status
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package helpful
  :commands
  (helpful-callable helpful-variable helpful-key helpful-command helpful-at-point)
  :hook
  (helpful-mode . (lambda ()
                    (set-window-dedicated-p (selected-window) t)))
  :custom
  (helpful-max-buffers 2)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap view-hello-file] . helpful-at-point))

(use-package consult
  ;;:after perspective
  :bind
  ([remap bookmark-jump] . consult-bookmark)
  :commands
  (consult-ripgrep
   consult-buffer
   consult-imenu
   consult-mode-command
   consult-yank-from-kill-ring)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; persp with consult
  ;;(consult-customize consult--source-buffer :hidden t :default nil)
  ;;(add-to-list 'consult-buffer-sources persp-consult-source)

  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "."))

(use-package embark
  ;; using bind* to override other bindings
  :bind*
  (("C-'" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :ensure nil
  :custom
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package nerd-icons-completion
  :after (:all nerd-icons marginalia)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     )))

(use-package compat :after vertico)
(use-package vertico
  :hook
  (on-first-input . vertico-mode)
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("M-<return>" . vertico-exit-input)
              ("ESC" . vertico-exit))
  :config
  ;; Add » before the selected completion.
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(defun nox/org-font-setup ()
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block-begin-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-block-end-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-document-info-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-code nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-cite nil
                      :underline nil)

  (set-face-attribute 'org-verbatim nil
                      :inherit 'variable-pitch)

  (set-face-attribute 'line-number-current-line nil
                      :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil
                      :inherit 'fixed-pitch)

  (set-face-attribute 'font-lock-string-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table    nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula  nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number  nil :inherit 'fixed-pitch)

  ;; disable large title
  (set-face-attribute 'org-document-title nil :height 'unspecified))

(use-package org-roam
  :commands
  (org-roam-capture
   org-roam-dailies-capture-today
   org-roam-node-find
   nox/org-roam-capture-tasks
   org-roam-goto-today)
  :custom
  (org-roam-directory nox/notes-directory)
  (org-roam-dailies-directory "Logs/")
  (org-roam-completion-everywhere t)

;; see help for format-time-string function for time templates
(org-roam-capture-templates
 '(("d" "default" item "%?%i"
    :if-new (file+head+olp
             "%<%Y%m%d%H%M%S>-${slug}.org"
             "#+title: ${title}\n#+date: [%<%Y-%m-%d %a %H:%M>]\n#+category: note"
             ("${title}"))
    :unnarrowed t
    :kill-buffer t)
   ("l" "linked" item "%?\n%a"
    :if-new (file+head+olp
             "%<%Y%m%d%H%M%S>-${slug}.org"
             "#+title: ${title}\n#+date: [%<%Y-%m-%d %a %H:%M>]\n#+category: note"
             ("${title}"))
    :unnarrowed t
    :kill-buffer t)))

(org-roam-dailies-capture-templates
 '(("d" "default" item "*%<%H:%M>* %?"
    :if-new (file+head+olp
             "%<%Y-%m-%d>.org"
             "#+title: %<%Y-%m-%d>\n#+date: [%<%Y-%m-%d %a %H:%M>]\n#+category: daily"
             ("%<%B %d, %Y>"))
    :unnarrowed t
    :kill-buffer t)
 ("l" "linked" item "*%<%H:%M>* %?\n%a"
  :if-new (file+head+olp
           "%<%Y-%m-%d>.org"
           "#+title: %<%Y-%m-%d>\n#+date: [%<%Y-%m-%d %a %H:%M>]\n#+category: daily"
           ("%<%B %d, %Y>"))
  :unnarrowed t
  :kill-buffer t)))

:config
(org-roam-db-autosync-mode)
;; auto insert mode
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(defun nox/org-roam-capture-tasks ()
  "Custom capture list for org roam"
  (interactive)
  (org-roam-capture-
   :node (org-roam-node-create)
   :templates '(
                ("t" "General Task"
                 entry "*** TODO %?"
                 :kill-buffer t
                 :if-new (file+head+olp
                          "Inbox/tasks.org"
                          "#+title: Tasks\n#+category: tasks"
                          ("Tasks" "General Tasks")))
                ("l" "Linked Task"
                 entry "*** TODO %?\n%a"
                 :kill-buffer t
                 :if-new (file+head+olp
                          "Inbox/tasks.org"
                          "#+title: Tasks\n#+category: tasks"
                          ("Tasks" "Linked Tasks")))
                ))))

(defun nox/org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun nox/org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun nox/org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (nox/org-show-properties)
    (nox/org-hide-properties)))

;; call org-hide-properties when inside org mode and org capture
(add-hook 'org-mode-hook 'nox/org-hide-properties)
(add-hook 'org-capture-mode-hook 'nox/org-hide-properties)

(with-eval-after-load 'org-roam

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (setq org-roam-node-display-template
        (concat "${title:80}" (propertize "${tags:20}" 'face 'org-tag))
        org-roam-node-annotation-function
        (lambda (node)
          (concat (org-roam-node-backlinkscount node) " "
                  (marginalia--time (org-roam-node-file-mtime node)))))
  )

(setq-default prettify-symbols-alist
              '(("#+begin_src emacs-lisp" . "")
                ("#+begin_src nix" . "")
                (":ATTACH:" . "🔗")
                ("#+begin_src" . "»")
                ("#+end_src" . "«")
                ("#+begin_quote" . "")
                ("#+end_quote" . "")
                ("#+RESULTS:" . "󰥤")
                (":tangle" . "󰯊")
                (":mkdirp yes" . "")
                ("lambda" . "λ")
                ("(interactive)" . "")))

(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Using RETURN to follow links in Org/Evil
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

;; Setting RETURN key in org-mode to follow links
(setq org-return-follows-link  t)

(use-package org
  :ensure nil
  :defer t
  :hook
  (org-mode . org-indent-mode)
  (org-mode . prettify-symbols-mode)
  (org-mode . (lambda () (display-line-numbers-mode -1)))
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  :custom
  (org-attach-id-dir "attachments/")
  (org-attach-use-inheritance t)
  (org-attach-method 'mv)
  (org-startup-with-inline-images t)
  (org-image-align 'center)
  (org-fontify-quote-and-verse-blocks t)
  (org-support-shift-select t)
  (org-hide-emphasis-markers t)
  :config
  (require 'docx-to-org)
  (nox/org-font-setup))

(use-package toc-org
    :hook (org-mode . toc-org-enable))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom (org-appear-autolinks t))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("nix" . "src nix"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  ;; turn off < auto pairing inside org-mode
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package svg-tag-mode
  :after org
  :hook
  (org-mode . svg-tag-mode)
  :custom
  (svg-tag-action-at-point 'edit)
  :config
  (set-face-attribute 'svg-tag-default-face nil :inherit 'fixed-pitch)
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  (setq svg-tag-tags
        `(
          ;; Task priority
          ("\\[#A\\]" .
           ((lambda (tag) (svg-tag-make "#A"  :face
                                        ;;`(:foreground ,(catppuccin-get-color 'red))
                                        :beg 2 :end -1 :margin 0))))
          ("\\[#B\\]" .
           ((lambda (tag) (svg-tag-make "#B"  :face
                                        ;;`(:foreground ,(catppuccin-get-color 'yellow))
                                        :beg 2 :end -1 :margin 0))))
          ("\\[#C\\]" .
           ((lambda (tag) (svg-tag-make "#C"  :face
                                        ;;`(:foreground ,(catppuccin-get-color 'green))
                                        :beg 2 :end -1 :margin 0))))

          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" .
           ((lambda (tag) (svg-tag-make tag  :inverse t
                                        :beg 7 :end -1
                                        :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" .
           ((lambda (tag) (svg-tag-make tag  :end -1
                                        :crop-left t))))

          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag) (svg-tag-make tag  :beg 1
                                        :end -1
                                        :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag) (svg-tag-make tag  :beg 1
                                        :inverse nil
                                        :crop-right t
                                        :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag) (svg-tag-make tag  :end -1
                                        :inverse t
                                        :crop-left t
                                        :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag) (svg-tag-make tag  :beg 1
                                        :end -1
                                        :margin 0
                                        :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag) (svg-tag-make tag  :beg 1
                                        :inverse nil
                                        :crop-right t
                                        :margin 0
                                        :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag) (svg-tag-make tag  :end -1
                                        :inverse t
                                        :crop-left t
                                        :margin 0
                                        :face 'org-date))))

          ;; Todos
          ("TODO" .
           ((lambda (tag) (svg-tag-make "TODO"
                                        :face
                                        ;;`(:foreground ,(catppuccin-get-color 'peach))
                                        :inverse t
                                        :margin 0))))
          ("READ" .
           ((lambda (tag) (svg-tag-make "READ"
                                        :face
                                        ;;`(:foreground ,(catppuccin-get-color 'rosewater))
                                        :inverse t
                                        :margin 0))))
          ("PLAN" .
           ((lambda (tag) (svg-tag-make "PLAN"
                                        :face
                                        ;;`(:foreground ,(catppuccin-get-color 'maroon))
                                        :inverse t
                                        :margin 0))))
          ("PROG" .
           ((lambda (tag) (svg-tag-make "PROG"
                                        :face
                                        ;;`(:foreground ,(catppuccin-get-color 'teal))
                                        :inverse t
                                        :margin 0))))
          ("WAIT" .
           ((lambda (tag) (svg-tag-make "WAIT"
                                        :face
                                        ;;`(:foreground ,(catppuccin-get-color 'red))
                                        :inverse t
                                        :margin 0))))
          ("DONE" .
           ((lambda (tag) (svg-tag-make "DONE"
                                        :face
                                        ;;`(:foreground ,(catppuccin-get-color 'green))
                                        :inverse t
                                        :margin 0))))
          ("CANC" .
           ((lambda (tag) (svg-tag-make "CANC"
                                        :face
                                        ;;`(:foreground
                                        ;;  ,(catppuccin-get-color 'overlay0))
                                        :inverse t
                                        :margin 0)))))))

(use-package visual-fill-column
  :hook
  (org-mode . visual-fill-column-mode)
  (org-agenda-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

(setq eshell-rc-script (concat nox/emacs-directory "eshell/profile")
      eshell-aliases-file (concat nox/emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(add-hook 'eshell-mode-hook (lambda ()
                              (hide-mode-line-mode)))

(use-package eshell-toggle
  :commands eshell-toggle
  :custom
  (eshell-toggle-run-command nil)
  (eshell-toggle-size-fraction 2))

(use-package vterm
  :commands (vterm vterm-toggle)
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  (vterm-mode . hide-mode-line-mode)
  :custom
  (vterm-shell "zsh -c nu"))

(use-package vterm-toggle :commands vterm-toggle)

(use-package undo-fu
  :after evil
  :config
  (setq undo-limit 67108864)          ; 64mb.
  (setq undo-strong-limit 100663296)  ; 96mb.
  (setq undo-outer-limit 1006632960)) ; 960mb.

(use-package undo-fu-session
  :after undo-fu
  :config
  (undo-fu-session-global-mode)
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :commands vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t))

(with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "C-M-u") 'vundo))
