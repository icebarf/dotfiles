(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-org-config))

(use-package eglot
  :ensure t
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure))

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
  :config
  (projectile-mode +1)
  (define-key xah-fly-command-map (kbd "SPC w p") 'projectile-command-map))

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
  :ensure t
  :defer t
  :init
  (treemacs-set-width 20))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right-bottom)
  ;;(which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  ;; daemon mode fixup
  (defun my/server-fix-up()
    (xah-fly-keys-set-layout "qwerty")
    (xah-fly-keys t))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'my/server-fix-up))
  
  ;; random `w'orkspace related keybinds
  (define-key xah-fly-command-map (kbd "SPC w t") 'treemacs)
  (define-key xah-fly-command-map (kbd "SPC w j") 'xref-find-references)  
  ;; `v' prefix keybinds - think variables or identifiers in code.
  
  ;; eglot
  (define-key xah-fly-command-map (kbd "SPC v r") 'eglot-rename)
  (define-key xah-fly-command-map (kbd "SPC v d") 'eglot-find-declaration)
  (define-key xah-fly-command-map (kbd "SPC v i") 'eglot-find-implementation)
  (define-key xah-fly-command-map (kbd "SPC v t") 'eglot-find-typeDefinition)
  )

(use-package yasnippet
  :ensure t
  :init
  (use-package yasnippet-snippets
    :ensure t)
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
   '(projectile-ripgrep yasnippet-snippets yasnippet which-key treemacs-tab-bar treemacs-projectile treemacs rainbow-delimiters projectile markdown-mode doom-themes doom-modeline company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
