;; random `w'orkspace related keybinds
(define-key xah-fly-command-map (kbd "SPC w j") 'xref-find-references)  
(define-key xah-fly-command-map (kbd "SPC w t") 'treemacs)

;; eglot
(define-key xah-fly-command-map (kbd "SPC E r") 'eglot-rename)
(define-key xah-fly-command-map (kbd "SPC E d") 'eglot-find-declaration)
(define-key xah-fly-command-map (kbd "SPC E i") 'eglot-find-implementation)
(define-key xah-fly-command-map (kbd "SPC E t") 'eglot-find-typeDefinition)
(define-key xah-fly-command-map (kbd "SPC E f") 'eglot-format-buffer)
(define-key xah-fly-command-map (kbd "SPC E F") 'eglot-format)

;; flymake
(define-key xah-fly-command-map (kbd "SPC F n") 'flymake-goto-next-error)
(define-key xah-fly-command-map (kbd "SPC F p") 'flymake-goto-previous-error)
(define-key xah-fly-command-map (kbd "SPC F b") 'flymake-show-buffer-diagnostics)
(define-key xah-fly-command-map (kbd "SPC F d") 'flymake-show-project-diagnostics)
