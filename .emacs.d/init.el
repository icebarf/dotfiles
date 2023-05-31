(use-package emacs
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  :config
  ;; window styling
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tab-bar-mode 1)
  ;; electric pair mode inserts pairs of all bracket-like characters
  (electric-pair-mode 1)
  ;; Backup Management
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))))


;; use package
(use-package use-package
  :ensure t
  :config
  (setq use-package-always-ensure t))

;; theme configuration
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

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

  ;; enable xah-fly-keys command mode by default if running in server/daemon mode
  :if (daemonp)
  :config
  (defun my/server-fix-up()
    (xah-fly-keys-set-layout "qwerty")
    (xah-fly-keys t))
  :hook
  (server-after-make-frame . my/server-fix-up))

;; smex stuff
(use-package smex
  :ensure t
  :bind
  ([remap execute-extended-command] . 'smex))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(clang-format xah-fly-keys smex doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
