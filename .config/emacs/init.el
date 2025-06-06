(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.config/emacs/lisp/")
(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package avy
  :ensure t)

(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin t)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

(use-package dap-mode
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-column-zero-based t)
  (setq doom-modeline-percent-position '(-3 "%p"))
  (setq doom-modeline-position-line-format '("%l:%c"))
  (setq doom-modeline-position-column-format '("C%c"))
  (setq doom-modeline-position-column-line-format '("%l:%c"))

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil))

(use-package helm
  :after protectile treemacs lsp-treemacs
  :ensure t
  :requires helm-lsp helm-xref
  :config
  (helm-mode 1)
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini)
)

(use-package helm-lsp
  :ensure t)

(use-package helm-xref
  :ensure t)

(use-package lsp-mode
  :after treemacs company
  :ensure t
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024)
	treemacs-space-between-root-nodes nil
	company-idle-delay 0.0
	company-minimum-prefix-length 1
	lsp-idle-delay 0.1
	lsp-clients-clangd-args '("--query-driver=/home/ice/x-tools/i686-unknown-elf/bin/i686-unknown-elf-g*" "-log=verbose")
	)
  )

(use-package lsp-treemacs
  :after treemacs projectile
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package projectile
  :after xah-fly-keys
  :ensure t
  :init
  (setq projectile-project-search-path '("~/Projects/c++/"
					 "~/Projects/c/"
					 "~/Projects/gsoc/"
					 "~/Projects/html/"
					 "~/Projects/Linux/"
					 "~/Projects/ms/"
					 "~/Projects/os/"
					 "~/Projects/py/"
					 "~/Projects/regex/"
					 "~/Projects/site/"
					 "~/Projects/Web/"
					 "~/Projects/x86/"
					 ))
  :config
  (projectile-mode +1)
  (define-key xah-fly-command-map (kbd "SPC P") 'projectile-command-map)
  )

(use-package projectile-ripgrep
  :after (projectile ripgrep)
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package ripgrep
  :ensure t)

(use-package treemacs
  :after winum xah-fly-keys
  :ensure t
  :config
  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  ;; xah-fly-keys keybindings
  (define-key xah-fly-command-map (kbd "SPC t 0") 'treemacs-select-window)
  (define-key xah-fly-command-map (kbd "SPC t 1") 'treemacs-delete-other-windows)
  (define-key xah-fly-command-map (kbd "SPC t t") 'treemacs)
  (define-key xah-fly-command-map (kbd "SPC t d") 'treemacs-select-directory)
  (define-key xah-fly-command-map (kbd "SPC t B") 'treemacs-bookmark)
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(treemacs-start-on-boot)

(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right-bottom)
  ;;(which-key-setup-side-window-bottom)
  (which-key-mode 1)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (defvar xah-major-mode-leader-key nil "Global leader key for major modes.
Value should be the same format that `kbd' returns.
e.g. (kbd \"TAB\") or (kbd \"<f9>\") or (kbd \"C-c\")")

  (when (not xah-major-mode-leader-key)
    (setq xah-major-mode-leader-key (kbd "TAB")))
  
  (defvar xah-org-leader-map nil "A keymap for all `org-mode' keybinding.")

  (defun xah-set-org-mode-keys ()
    (interactive)
    (progn
      (define-prefix-command 'xah-org-leader-map)

      (define-key xah-org-leader-map (kbd "TAB") #'org-cycle)

      (define-key xah-org-leader-map (kbd "SPC RET") #'org-ctrl-c-ret)
      (define-key xah-org-leader-map (kbd "SPC TAB") #'org-ctrl-c-tab)
      ;; (define-key xah-org-leader-map (kbd "<tab>") #'org-cycle-force-archived)

      (define-key xah-org-leader-map (kbd "!") #'org-time-stamp-inactive)
      (define-key xah-org-leader-map (kbd "#") #'org-update-statistics-cookies)
      (define-key xah-org-leader-map (kbd "$") #'org-archive-subtree)
      (define-key xah-org-leader-map (kbd "%") #'org-mark-ring-push)
      (define-key xah-org-leader-map (kbd "&") #'org-mark-ring-goto)
      (define-key xah-org-leader-map (kbd "'") #'org-edit-special)
      (define-key xah-org-leader-map (kbd "*") #'org-list-make-subtree)
      (define-key xah-org-leader-map (kbd "+") #'org-table-sum)
      (define-key xah-org-leader-map (kbd ",") #'org-priority)
      (define-key xah-org-leader-map (kbd "-") #'org-ctrl-c-minus)
      (define-key xah-org-leader-map (kbd ".") #'org-time-stamp)
      (define-key xah-org-leader-map (kbd "/") #'org-sparse-tree)
      (define-key xah-org-leader-map (kbd ":") #'org-toggle-fixed-width)
      (define-key xah-org-leader-map (kbd ";") #'org-toggle-comment)
      (define-key xah-org-leader-map (kbd "<") #'outline-promote)
      (define-key xah-org-leader-map (kbd "=") #'org-table-eval-formula)
      (define-key xah-org-leader-map (kbd ">") #'outline-demote)
      (define-key xah-org-leader-map (kbd "?") #'org-table-field-info)
      (define-key xah-org-leader-map (kbd "@") #'org-mark-subtree)
      (define-key xah-org-leader-map (kbd "[") #'org-agenda-file-to-front)
      (define-key xah-org-leader-map (kbd "\\") #'org-match-sparse-tree)
      (define-key xah-org-leader-map (kbd "]") #'org-remove-file)
      (define-key xah-org-leader-map (kbd "^") #'org-up-element)
      (define-key xah-org-leader-map (kbd "_") #'org-down-element)
      (define-key xah-org-leader-map (kbd "`") #'org-table-edit-field)
      (define-key xah-org-leader-map (kbd "{") #'org-table-toggle-formula-debugger)
      (define-key xah-org-leader-map (kbd "|") #'org-table-create-or-convert-from-region)
      (define-key xah-org-leader-map (kbd "}") #'org-table-toggle-coordinate-overlays)
      (define-key xah-org-leader-map (kbd "~") #'org-table-create-with-table.el)

      (define-key xah-org-leader-map (kbd "a") #'org-attach)
      (define-key xah-org-leader-map (kbd "b") #'org-backward-heading-same-level)
      (define-key xah-org-leader-map (kbd "c") #'org-ctrl-c-ctrl-c)
      (define-key xah-org-leader-map (kbd "d") #'org-deadline)
      (define-key xah-org-leader-map (kbd "e") #'org-export-dispatch)
      (define-key xah-org-leader-map (kbd "f") #'org-forward-heading-same-level)

      (define-key xah-org-leader-map (kbd "g ,") #'org-date-from-calendar)
      (define-key xah-org-leader-map (kbd "g t") #'org-insert-structure-template)
      (define-key xah-org-leader-map (kbd "g .") #'org-goto-calendar)
      (define-key xah-org-leader-map (kbd "g 6") #'org-sort)
      (define-key xah-org-leader-map (kbd "g a") #'orgtbl-ascii-plot)
      (define-key xah-org-leader-map (kbd "g g") #'org-plot/gnuplot)
      (define-key xah-org-leader-map (kbd "g m b") #'org-previous-block)
      (define-key xah-org-leader-map (kbd "g m f") #'org-next-block)
      (define-key xah-org-leader-map (kbd "g m l") #'org-insert-last-stored-link)
      (define-key xah-org-leader-map (kbd "g m w") #'org-refile-copy)
      (define-key xah-org-leader-map (kbd "g s") #'org-ctrl-c-star)

      ;; h i

      (define-key xah-org-leader-map (kbd "j") #'org-goto)
      (define-key xah-org-leader-map (kbd "k") #'org-kill-note-or-show-branches)
      (define-key xah-org-leader-map (kbd "l") #'org-insert-link)

      (define-key xah-org-leader-map (kbd "m l") #'org-insert-all-links)
      (define-key xah-org-leader-map (kbd "m w") #'org-refile-reverse)

      (define-key xah-org-leader-map (kbd "n") #'outline-next-visible-heading)
      (define-key xah-org-leader-map (kbd "o") #'org-open-at-point)
      (define-key xah-org-leader-map (kbd "p") #'outline-previous-visible-heading)
      (define-key xah-org-leader-map (kbd "q") #'org-set-tags-command)
      (define-key xah-org-leader-map (kbd "r") #'org-fold-reveal)
      (define-key xah-org-leader-map (kbd "s") #'org-schedule)
      (define-key xah-org-leader-map (kbd "t") #'org-todo)
      (define-key xah-org-leader-map (kbd "u") #'outline-up-heading)
      (define-key xah-org-leader-map (kbd "v C-M-h") #'org-babel-mark-block)
      (define-key xah-org-leader-map (kbd "v I") #'org-babel-view-src-block-info)
      (define-key xah-org-leader-map (kbd "v TAB") #'org-babel-view-src-block-info)
      (define-key xah-org-leader-map (kbd "v a") #'org-babel-sha1-hash)
      (define-key xah-org-leader-map (kbd "v b") #'org-babel-execute-buffer)
      (define-key xah-org-leader-map (kbd "v c") #'org-babel-check-src-block)
      (define-key xah-org-leader-map (kbd "v d") #'org-babel-demarcate-block)
      (define-key xah-org-leader-map (kbd "v e") #'org-babel-execute-maybe)
      (define-key xah-org-leader-map (kbd "v f") #'org-babel-tangle-file)
      (define-key xah-org-leader-map (kbd "v g") #'org-babel-goto-named-src-block)
      (define-key xah-org-leader-map (kbd "v h") #'org-babel-describe-bindings)
      (define-key xah-org-leader-map (kbd "v i") #'org-babel-lob-ingest)
      (define-key xah-org-leader-map (kbd "v j") #'org-babel-insert-header-arg)
      (define-key xah-org-leader-map (kbd "v k") #'org-babel-remove-result-one-or-many)
      (define-key xah-org-leader-map (kbd "v l") #'org-babel-load-in-session)
      (define-key xah-org-leader-map (kbd "v n") #'org-babel-next-src-block)
      (define-key xah-org-leader-map (kbd "v o") #'org-babel-open-src-block-result)
      (define-key xah-org-leader-map (kbd "v p") #'org-babel-previous-src-block)
      (define-key xah-org-leader-map (kbd "v r") #'org-babel-goto-named-result)
      (define-key xah-org-leader-map (kbd "v s") #'org-babel-execute-subtree)
      (define-key xah-org-leader-map (kbd "v t") #'org-babel-tangle)
      (define-key xah-org-leader-map (kbd "v u") #'org-babel-goto-src-block-head)
      (define-key xah-org-leader-map (kbd "v v") #'org-babel-expand-src-block)
      (define-key xah-org-leader-map (kbd "v x") #'org-babel-do-key-sequence-in-edit-buffer)
      (define-key xah-org-leader-map (kbd "v z") #'org-babel-switch-to-session-with-code)
      (define-key xah-org-leader-map (kbd "w") #'org-refile)
      (define-key xah-org-leader-map (kbd "x !") #'org-reload)
      (define-key xah-org-leader-map (kbd "x ,") #'org-timer-pause-or-continue)
      (define-key xah-org-leader-map (kbd "x -") #'org-timer-item)
      (define-key xah-org-leader-map (kbd "x .") #'org-timer)
      (define-key xah-org-leader-map (kbd "x 0") #'org-timer-start)
      (define-key xah-org-leader-map (kbd "x ;") #'org-timer-set-timer)
      (define-key xah-org-leader-map (kbd "x <") #'org-agenda-set-restriction-lock)
      ;; (define-key xah-org-leader-map (kbd "x >") #'org-agenda-remove-restriction-lock)
      (define-key xah-org-leader-map (kbd "x @") #'org-cite-insert)
      (define-key xah-org-leader-map (kbd "x A") #'org-archive-to-archive-sibling)
      (define-key xah-org-leader-map (kbd "x C-M-v") #'org-redisplay-inline-images)
      (define-key xah-org-leader-map (kbd "x E") #'org-inc-effort)
      (define-key xah-org-leader-map (kbd "x G") #'org-feed-goto-inbox)
      (define-key xah-org-leader-map (kbd "x I") #'org-info-find-node)
      (define-key xah-org-leader-map (kbd "x M-w") #'org-copy-special)
      (define-key xah-org-leader-map (kbd "x P") #'org-set-property-and-value)
      (define-key xah-org-leader-map (kbd "x TAB") #'org-clock-in)
      (define-key xah-org-leader-map (kbd "x [") #'org-reftex-citation)
      (define-key xah-org-leader-map (kbd "x \\") #'org-toggle-pretty-entities)
      (define-key xah-org-leader-map (kbd "x _") #'org-timer-stop)
      (define-key xah-org-leader-map (kbd "x a") #'org-toggle-archive-tag)
      (define-key xah-org-leader-map (kbd "x b") #'org-tree-to-indirect-buffer)
      (define-key xah-org-leader-map (kbd "x c") #'org-clone-subtree-with-time-shift)
      (define-key xah-org-leader-map (kbd "x d") #'org-insert-drawer)
      (define-key xah-org-leader-map (kbd "x e") #'org-set-effort)
      (define-key xah-org-leader-map (kbd "x f") #'org-footnote-action)
      (define-key xah-org-leader-map (kbd "x g") #'org-feed-update-all)
      (define-key xah-org-leader-map (kbd "x o") #'org-toggle-ordered-property)
      (define-key xah-org-leader-map (kbd "x p") #'org-set-property)
      (define-key xah-org-leader-map (kbd "x q") #'org-toggle-tags-groups)
      (define-key xah-org-leader-map (kbd "x t a") #'org-archive-subtree-default)
      (define-key xah-org-leader-map (kbd "x t b") #'org-toggle-checkbox)
      (define-key xah-org-leader-map (kbd "x t c") #'org-columns)
      (define-key xah-org-leader-map (kbd "x t d") #'org-clock-display)
      (define-key xah-org-leader-map (kbd "x t f") #'org-emphasize)
      (define-key xah-org-leader-map (kbd "x t j") #'org-clock-goto)
      (define-key xah-org-leader-map (kbd "x t l") #'org-latex-preview)
      (define-key xah-org-leader-map (kbd "x t n") #'org-next-link)
      (define-key xah-org-leader-map (kbd "x t o") #'org-clock-out)
      (define-key xah-org-leader-map (kbd "x t p") #'org-previous-link)
      (define-key xah-org-leader-map (kbd "x t q") #'org-clock-cancel)
      (define-key xah-org-leader-map (kbd "x t r") #'org-toggle-radio-button)
      (define-key xah-org-leader-map (kbd "x t s") #'org-archive-subtree)
      (define-key xah-org-leader-map (kbd "x t t") #'org-toggle-time-stamp-overlays)
      (define-key xah-org-leader-map (kbd "x t u") #'org-dblock-update)
      (define-key xah-org-leader-map (kbd "x t v") #'org-toggle-inline-images)
      (define-key xah-org-leader-map (kbd "x t w") #'org-cut-special)
      (define-key xah-org-leader-map (kbd "x t x") #'org-clock-in-last)
      (define-key xah-org-leader-map (kbd "x t y") #'org-paste-special)
      (define-key xah-org-leader-map (kbd "x t z") #'org-resolve-clocks)
      (define-key xah-org-leader-map (kbd "x v") #'org-copy-visible)
      (define-key xah-org-leader-map (kbd "x x") #'org-dynamic-block-insert-dblock)
      (define-key xah-org-leader-map (kbd "y") #'org-evaluate-time-range)
      (define-key xah-org-leader-map (kbd "z") #'org-add-note)
      )

    (define-key
     org-mode-map
     (if (boundp 'xah-major-mode-leader-key)
	 xah-major-mode-leader-key
       (kbd "TAB"))
     xah-org-leader-map)
    ;;
    )

(add-hook 'org-mode-hook 'xah-set-org-mode-keys)

;; daemon mode fixup
(defun my/server-fix-up()
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys t))
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'my/server-fix-up))

;; random workspace related keybinds
(define-key xah-fly-command-map (kbd "SPC w j") 'xref-find-references)

;; flymake
(define-key xah-fly-command-map (kbd "SPC F n") 'flymake-goto-next-error)
(define-key xah-fly-command-map (kbd "SPC F p") 'flymake-goto-prev-error)
(define-key xah-fly-command-map (kbd "SPC F b") 'flymake-show-buffer-diagnostics)
(define-key xah-fly-command-map (kbd "SPC F d") 'flymake-show-project-diagnostics)

;; helm -- remap find-file in xah-fly-keys to helm-find-file as well
(define-key xah-fly-command-map (kbd "SPC i e") 'helm-find-files)
  )

(use-package winum
  :ensure t
  :config
  (winum-mode 1))

(use-package yasnippet
  :after dap-cpptools
  :ensure t
  :init
  (use-package yasnippet-snippets
    :ensure t)
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode 1))

;; Emacs customization

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; disable bars
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tab-bar-mode nil)

  ;; write in pairs
  (electric-pair-mode 1)

  ;; font config
  (add-to-list 'default-frame-alist
               '(font . "Iosevka Nerd Font-14"))
  (set-face-attribute 'default t :font "Iosevka Nerd Font-14")
  (set-face-attribute 'default nil :font "Iosevka Nerd Font-14")

  ;; utf-8-unix style coding always (newlines at the end)
  (set-buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy bug-hunter catppuccin-theme company dap-cpptools dap-mode
	 doom-modeline doom-themes gnu-elpa-keyring-update helm
	 helm-lsp helm-xref lsp-mode lsp-treemacs magit markdown-mode
	 projectile projectile-ripgrep rainbow-delimiters
	 treemacs-evil treemacs-icons-dired treemacs-magit
	 treemacs-persp treemacs-projectile treemacs-tab-bar which-key
	 winum xah-fly-keys yasnippet yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
