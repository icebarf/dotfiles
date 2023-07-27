(use-package emacs
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
	(package-refresh-contents)

  :config
  ;; window styling
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tab-bar-mode 1)

  (add-to-list 'default-frame-alist '(font . "Comic Mono-13"))
  (set-frame-font "Comic Mono-13" nil t)
  
  ;; electric pair mode inserts pairs of all bracket-like characters
  (electric-pair-mode 1)

  (setq-default tab-width 2)

	;; yes or no
	(fset 'yes-or-no-p 'y-or-n-p)

	(setq org-export-backends (quote (
																		html
																		man
																		)))

  ;; Backup Management
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))))

;; use package
(use-package use-package
  :ensure t
  :config
  (setq use-package-always-ensure t))

;; theme configuration
;(use-package doom-themes
;  :config
;  (load-theme 'doom-one t))


(use-package catppuccin-theme
	:config
	(load-theme 'catppuccin t))

;; matrix client lmao
(use-package ement)

;; keybinding configuration
(use-package xah-fly-keys
  :config
  ;; make escape key do both activate command mode and cancel
  ;; from Egor Maltsev
  (progn
    (defun xah-fly-keys-escape ()
      (interactive)
      (when (region-active-p)
	(deactivate-mark))
      (when (active-minibuffer-window)
	(abort-recursive-edit)))
    (define-key xah-fly-command-map (kbd "<escape>")     'xah-fly-keys-escape))
  (progn
    (defvar xah-fly-keys-fast-keyseq-timeout 200)

    (defun xah-fly-keys-tty-ESC-filter (map)
      (if (and (equal (this-single-command-keys) [?\e])
	       (sit-for (/ xah-fly-keys-fast-keyseq-timeout 1000.0)))
	  [escape] map))

    (defun xah-fly-keys-lookup-key (map key)
      (catch 'found
	(map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))
    (defun xah-fly-keys-catch-tty-ESC ()
      "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
      (when (memq (terminal-live-p (frame-terminal)) '(t pc))
	(let ((esc-binding (xah-fly-keys-lookup-key input-decode-map ?\e)))
	  (define-key input-decode-map
		      [?\e] `(menu-item "" ,esc-binding :filter xah-fly-keys-tty-ESC-filter)))))

    (xah-fly-keys-catch-tty-ESC)
    (define-key key-translation-map (kbd "ESC") (kbd "<escape>")))

  ;; disable control and meta key stuff
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key nil)
  
  :bind-keymap
  ("<f7>" . xah-fly-leader-key-map)

  :bind
  (:map xah-fly-command-map
	("SPC w k" . kill-buffer))
  (:map xah-fly-command-map
	("SPC w n" . tab-new))
  (:map xah-fly-command-map
	("SPC w c" . tab-close))

  ;; enable xah-fly-keys command mode by default
  :if (daemonp)
  :config
  (defun my/xah-fly-activate()
    (xah-fly-keys-set-layout "qwerty")
    (xah-fly-keys t))
  :hook
  (server-after-make-frame . my/xah-fly-activate))


(use-package smex
  :bind
  ([remap execute-extended-command] . 'smex))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

(use-package company
  :config
  (company-mode nil)
  :hook
  (after-init-hook . global-company-mode)
  :if (daemonp)
  :hook (server-after-make-frame . global-company-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :if (daemonp)
  :hook
  (server-after-make-frame . projectile-mode))

(use-package which-key
  :config
  (which-key-mode)
  :if (daemonp)
  :hook
  (server-after-make-frame . which-key-mode))

(use-package treemacs)

(use-package lsp-treemacs
  :after treemacs)

(use-package lsp-mode
  :after lsp-treemacs
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (lsp-mode . lsp-which-key-integration))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

(use-package ebnf2ps)

(use-package rustic)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(catpuccin-theme catppuccin-theme rustic ox-html ox-man company-org-block ebnf-mode clang-format+ projectile lsp-treemacs which-key flycheck multiple-cursors company clang-format ement xah-fly-keys smex doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
