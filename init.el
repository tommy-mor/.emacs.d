;; https://github.crookster.org/switching-to-straight.el-from-emacs-26-builtin-package.el/
;; https://countvajhula.com/2020/12/27/turn-your-emacs-d-into-an-emacs-distribution-with-straight-el/

(message "i am loading my ~/.emacs.d/init.el")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(scroll-bar-mode -1)




(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))


(use-package avy
  :config
  (setq avy-keys '(?a ?r ?s ?t ?h ?n ?e ?i)))

(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<esc>") ()))

;(use-package evil-snipe :config (evil-snipe-mode 1) (evil-snipe-override-mode 1))


(use-package evil-leader :config
  (global-evil-leader-mode 1)
  
  (evil-leader/set-leader ",")
  (evil-leader/set-key
	"f" 'helm-find-files
	"b" 'helm-mini
	"k" 'helm-show-kill-ring
	"a" 'company-mode
	;; TODO have evil leader with hold comma, then press tab stuff
	"TAB" 'mode-line-other-buffer
	"v" 'er/expand-region
	"c" 'comment-region
	"r" 'revert-buffer-no-confirm
	"q" 'recompile ;; kill-this-buffer
	;;"R" () ;; kill-this-buffer
	"d" 'kill-this-buffer
	"g" 'helm-projectile-ag 
	"e" 'string-edit-at-point
	"o" 'helm-occur
	"t" 'avy-goto-word-1
	;;"p" 'font-lock-fontify-buffer
	"p" 'helm-projectile))

(use-package evil-surround :config (global-evil-surround-mode 1))

(use-package undo-tree :config
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree))

(setq evil-normal-state-tag   (propertize " COMMAND " 'face '((:background "dark khaki" :foreground "black")))
	  evil-emacs-state-tag    (propertize "  EMACS  " 'face '((:background "turquoise" :foreground "black")))
	  evil-insert-state-tag   (propertize " ------- " 'face '((:background "dark sea green" :foreground "black")))
	  evil-replace-state-tag  (propertize " REPLACE " 'face '((:background "dark orange" :foreground "black")))
	  evil-motion-state-tag   (propertize "  MOTION " 'face '((:background "khaki" :foreground "black")))
	  evil-visual-state-tag   (propertize "  VISUAL " 'face '((:background "light salmon" :foreground "black")))
	  evil-operator-state-tag (propertize " OPERATE " 'face '((:background "sandy brown" :foreground "black")))
	  evil-symex-state-tag    (propertize "  SYMEX  " 'face '((:background "purple" :foreground "black"))))


(use-package evil-smartparens)
(use-package smartparens :config
  (smartparens-global-mode 1)
  (evil-smartparens-mode 1)
  (sp-local-pair 'Shell "'" nil :actions nil))

(use-package helm :config 
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (helm-autoresize-mode 0)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-recentf-fuzzy-match t))

(use-package helm-projectile)
(use-package tree-sitter)
(use-package tree-sitter-langs
  :config
  (add-to-list 'tree-sitter-load-path "/home/tommy/programming/brain/bin/")
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))



;; small modes
(use-package hl-todo :config (global-hl-todo-mode 1))
(use-package anzu :config (global-anzu-mode 1))
(use-package expand-region)
(use-package company)
(use-package ag)
(use-package helm-ag)
(use-package go-mode)
(use-package evil-colemak-basics :config (global-evil-colemak-basics-mode 1))
(use-package exec-path-from-shell :config (exec-path-from-shell-initialize))
(use-package leuven-theme)
(use-package wakatime-mode :config (global-wakatime-mode 1))



;; -------------------------- OPTIONS

(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(global-display-line-numbers-mode 1)
(setq ring-bell-function 'ignore)

(blink-cursor-mode 0)

(setq-default tab-width 4)

(setq indent-tabs-mode 1)

(windmove-default-keybindings)

(load-theme 'leuven t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(global-hl-line-mode 0)

(setq nrepl-use-ssh-fallback-for-remote-hosts 't)

(setq show-paren-delay 0)
(show-paren-mode 1)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package flycheck-clj-kondo)
(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))


(use-package leuven-theme)


;; (setq initial-buffer-choice "~/programming/tdsl/test/todo.tdsl")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-merge-sessions 'project)
 '(custom-safe-themes
   '("cfe4d36ed4cf00a541f7ba0deb38c94808c13a3e4c717f07bc3b9c866670e8d1" "474513bacf33a439da7b9a5df1dd11a277929d8480752675fc7d5f3816d8fdef" "c505ae23385324c21821b24c9cc1d68d8da6f3cfb117eb18826d146b8ec01b15" default))
 '(safe-local-variable-values
   '((eval progn
		   (local-set-key
			(kbd "C-c C-r")
			(lambda nil
			  (interactive)
			  (cider-interactive-eval "(main-default)" nil nil
									  (cider--nrepl-pr-request-map)))))
	 (eval progn
		   (local-set-key
			(kbd "C-c C-r")
			(lambda nil
			  (interactive)
			  (cider-interactive-eval "(require 'development) (in-ns 'development) (restart)" nil nil
									  (cider--nrepl-pr-request-map)))))
	 (eval progn
		   (local-set-key
			(kbd "C-c C-r")
			(lambda nil
			  (interactive)
			  (cider-interactive-eval "(in-ns 'development) (restart)" nil nil
									  (cider--nrepl-pr-request-map)))))
	 (eval progn
		   (local-set-key
			(kbd "C-c C-r")
			(lambda nil
			  (interactive)
			  (cider-interactive-eval "(+ 3 3)" nil nil
									  (cider--nrepl-pr-request-map)))))
	 (eval progn
		   (local-set-key
			(kbd "C-c C-c")
			(lambda nil
			  (interactive)
			  (cider-interactive-eval "(+ 3 3)" nil nil
									  (cider--nrepl-pr-request-map)))))))
 '(undo-tree-auto-save-history nil)
 '(wakatime-api-key "b93ccd46-94c9-4b96-a195-4e0205b9cc36")
 '(warning-suppress-types '((use-package) (comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "C-9") 'sp-split-sexp)
(global-set-key (kbd "C-(") 'sp-backward-barf-sexp)
(global-set-key (kbd "C-)") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-9") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-0") 'sp-forward-slurp-sexp)
(global-set-key (kbd "s-k") 'kill-buffer)

;;(helm-posframe-enable)
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height))) 

(defun scroll-down-half ()         
  (interactive)                    
  (scroll-down (window-half-height)))


(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-n") 'scroll-up-half)
  (define-key evil-normal-state-map (kbd "C-e") 'scroll-down-half)
										; overwrites top of window keybind
										; have this be 'window-next' and 'window prev'
  (define-key evil-normal-state-map (kbd "H") 'windmove-left) 
  (define-key evil-normal-state-map (kbd "I") 'windmove-right))

; tree sitter bs
(setq er/try-expand-list (append er/try-expand-list
								 '(tree-sitter-mark-bigger-node)))

; (load-file "~/.emacs.d/treenav.el")
(setq undo-tree-visualizer-diff nil)

(save-place-mode t)


(defun cider-eval-and-replace ()
  (interactive)
  (goto-char (cadr (cider-sexp-at-point 'bounds)))
  (cider-eval-last-sexp-and-replace))

(defun cider-eval-recalling (&optional debug-it)
  (interactive "P")
  (let ((inline-debug (eq 16 (car-safe debug-it)))
		(thing-to-send (concat "(clojure.test/run-test-var #'" (cider-symbol-at-point) ")") ) )
	(cider-interactive-eval
	 thing-to-send
	 nil
	 (cider-defun-at-point 'bounds)
	 (cider--nrepl-pr-request-map))))


;(load-file "~/programming/examplegarden/bind.el")
(use-package symex-core
  :straight
  (symex-core
   :local-repo "~/programming/symex.el"
   :type git
   :files ("symex-core/symex*.el")))

(use-package symex
  :after (symex-core)
  :straight
  (symex
   :local-repo "~/programming/symex.el"
   :type git
   :files ("symex/symex*.el" "symex/doc/*.texi" "symex/doc/figures"))
  :config
  (symex-mode 1)
  (global-set-key (kbd "s-;") #'symex-mode-interface)) ;

(use-package symex-evil
  :after (symex evil)
  :straight
  (symex-evil
   :type git
   :local-repo "~/programming/symex.el"
   :files ("symex-evil/symex*.el"))
  :config
  (symex-evil-mode 1))

(use-package symex-ide
  :after (symex)
  :straight
  (symex-ide
   :type git

   :local-repo "~/programming/symex.el"
   :files ("symex-ide/symex*.el"))
  :config
  (symex-ide-mode 1))





;; Place this in your init.el or equivalent config file
(eval-after-load 'symex
  '(progn
	 (lithium-define-keys symex-editing-mode
       ;; Bind 'u' to the command, and crucially, add the :exit flag.
       (("u" symex-insert-at-beginning :exit)))
	 
	 ;; Unbind the default Symex navigation keys that you are replacing.
     ;; This prevents the default 'h', 'j', 'k', 'l' from also being active.
	 (define-key symex-editing-mode-map (kbd "h") nil)
	 (define-key symex-editing-mode-map (kbd "j") nil)
	 (define-key symex-editing-mode-map (kbd "k") nil)
	 (define-key symex-editing-mode-map (kbd "l") nil)
	 

     ;; --- Your Custom Keybindings ---
     ;; This section maps your preferred keys to the Symex commands.

     ;; Basic Evil and Navigation
	 (define-key symex-editing-mode-map (kbd ":") #'evil-ex)
     ;; "l" is not a standard evil command for undo, but respecting your config.
     ;; The standard is "u". If you meant "u", you can change it here.
	 (define-key symex-editing-mode-map (kbd "l") #'evil-undo)

     ;; Your Colemak Navigation (hnei)
	 (define-key symex-editing-mode-map (kbd "h") #'symex-go-down)
	 (define-key symex-editing-mode-map (kbd "n") #'symex-go-forward)
	 (define-key symex-editing-mode-map (kbd "e") #'symex-go-backward)
	 (define-key symex-editing-mode-map (kbd "i") #'symex-go-up)
	 
	 ;; Leaping
	 (define-key symex-editing-mode-map (kbd "C-e") #'symex-leap-backward)
	 
	 (define-key symex-editing-mode-map (kbd "C-n") #'symex-leap-forward)

     ;; Structural Manipulation
	 (define-key symex-editing-mode-map (kbd "H") #'symex-raise) ; Note: paredit-raise-sexp is now symex-raise
	 (define-key symex-editing-mode-map (kbd "N") #'symex-shift-forward)
	 (define-key symex-editing-mode-map (kbd "E") #'symex-shift-backward)
	 (define-key symex-editing-mode-map (kbd "I") #'symex-wrap)

     ;; Traversal and Wrapping
	 (define-key symex-editing-mode-map (kbd "w") #'symex-traverse-forward)
	 (define-key symex-editing-mode-map (kbd "W") #'symex-wrap)

     ;; Branch Navigation
	 (define-key symex-editing-mode-map (kbd "C-h") #'symex-climb-branch)
	 (define-key symex-editing-mode-map (kbd "C-I") #'symex-descend-branch) ; Assuming uppercase I
	 (define-key symex-editing-mode-map (kbd "M-i") #'symex-goto-highest)
	 (define-key symex-editing-mode-map (kbd "M-h") #'symex-goto-lowest)

     ;; IDE / Evaluation Commands
	 (define-key symex-editing-mode-map (kbd "M-n") #'symex-evaluate)
	 (define-key symex-editing-mode-map (kbd "M-d") #'symex-describe) ; Note: This calls the generic symex-describe

     ;; IMPORTANT: These two Cider functions may have been removed or renamed in
     ;; recent versions of Cider. The common function now is `cider-eval-last-sexp-and-replace`.
     ;; If these bindings cause an error, you may need to update them.
     ;; (define-key symex-editing-mode-map (kbd "M-N") #'cider-eval-and-replace)
     ;; (define-key symex-editing-mode-map (kbd "D")   #'cider-eval-recalling)
     ))


(eval-after-load 'evil
  '(progn
     (message "Binding <escape> in normal mode to launch Symex...")
     (define-key evil-normal-state-map (kbd "<escape>") #'symex-mode-interface)
	 (define-key evil-insert-state-map (kbd "<escape>") #'symex-mode-interface)
	 
	 
	 ))




(use-package cider)


										; (load "$HOME/.lisp.el")
(setq line-number-mode t)
(setq column-number-mode t)
(setq visible-bell t)
(setq fill-column 70)
(setq default-major-mode 'text-mode)

(setq text-mode-hook
	  '(lambda () (auto-fill-mode 1)))

(add-hook 'text-mode 'turn-on-auto-fill)

(use-package atomic-chrome
  :config
  (require 'atomic-chrome)
  (atomic-chrome-start-server))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  ;; :config
  ;; (setq copilot-node-executable "/home/tommy/.nvm/versions/node/v17.9.1/bin/node")
  )

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(byte-recompile-directory (expand-file-name "~/.emacs.d/straight/build/symex") 0)




