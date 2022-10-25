
(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(column-number-mode 1)


(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(require 'package)
(add-to-list 'package-archives
			 '("melpa-stable" . "https://melpa.org/packages/") t)

(eval-when-compile ;; Following line is not needed if use-package.el is in ~/.emacs.d (require 'use-palllCKAGE)
  (require 'use-package))

(setq use-package-always-ensure t)

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
	"g" 'helm-grep-do-git-grep 
	"e" 'string-edit-at-point
	"s" 'aya-create
	"S" 'aya-expand
	"o" 'helm-occur
	"l" 'helm-locate
	"t" 'avy-goto-word-1
	"p" 'font-lock-fontify-buffer))

;"p" 'helm-projectile-find-file))
(use-package evil-surround :ensure t :config (global-evil-surround-mode 1))

(use-package undo-tree :config (global-undo-tree-mode) (evil-set-undo-system 'undo-tree))
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


(use-package expand-region)
(use-package company)
(use-package evil-colemak-basics
  :config
  (global-evil-colemak-basics-mode 1))

(use-package exec-path-from-shell :config (exec-path-from-shell-initialize))
(use-package leuven-theme)
;; (use-package wakatime-mode
;;   :config
;;   (global-wakatime-mode 1))



;; -------------------------- OPTIONS

(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(global-linum-mode 1)

(setq ring-bell-function 'ignore)

(blink-cursor-mode 0)

(setq-default tab-width 4)

(setq indent-tabs-mode 1)

(windmove-default-keybindings)

(load-theme 'deeper-blue t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(global-hl-line-mode 0)

(show-paren-mode 1)
(setq show-paren-delay 0)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package clojure-mode)

(use-package leuven-theme)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(undo-tree-auto-save-history nil)
 '(wakatime-api-key "b93ccd46-94c9-4b96-a195-4e0205b9cc36")
 '(wakatime-cli-path "/home/tommy/go/bin/wakatime-cli")
 '(warning-suppress-types '((comp))))

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

;; TODO copy this..
(require 'symex)


(defun cider-eval-and-replace ()
  (interactive)
  (goto-char (cadr (cider-sexp-at-point 'bounds)))
  (cider-eval-last-sexp-and-replace))

(setq symex--user-evil-keyspec
	  '((":" . evil-ex)
		("l" . evil-undo)
		
		("h" . symex-go-down)
		("n" . symex-go-forward)
		("e" . symex-go-backward)
		("i" . symex-go-up)

		("C-e" . symex-leap-backward)
		("C-n" . symex-leap-forward)
		
		("H" . paredit-raise-sexp)
		("N" . symex-shift-forward)
		("E" . symex-shift-backward)
		("I" . symex-wrap)

		("w" . symex-traverse-forward)
		("W" . symex-wrap)
		
		
		("C-h" . symex-climb-branch)
		("C-I" . symex-descend-branch)
		("M-i" . symex-goto-highest)
		("M-h" . symex-goto-lowest)
		("M-n" . symex-evaluate)
		("M-d" . cider-doc)
		("M-N" . cider-eval-and-replace)))


(symex-initialize)
(global-set-key (kbd "s-;") 'symex-mode-interface)

(setq symex-modal-backend 'evil)

(evil-define-key 'insert symex-mode-map
  (kbd "<escape>") 'symex-mode-interface)

(evil-define-key 'normal symex-mode-map
  (kbd "<escape>") 'symex-mode-interface)


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

(defun connect-remote ()
  (interactive)
  (dired "/ssh:root@143.198.100.76:/"))

(setq nrepl-use-ssh-fallback-for-remote-hosts t)
