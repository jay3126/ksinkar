;; redefining emacs keybindings to generic keyboard shortcuts
(cua-mode) ;; C-c and C-x are for copying and pasting if a  region has been selected other-wise they perform the normal emacs functions
(global-set-key (kbd "C-l") 'goto-line)			;; for going to line number
(global-set-key (kbd "C-s") 'save-buffer)		;; for saving a buffer
(global-set-key (kbd "C-f") 'isearch-forward)	        ;; for finding in a buffer
;; once you entered the text in C-f, you can just go through the finds using the pgup and pgdwn key
(define-key isearch-mode-map [next]
  'isearch-repeat-forward)
(define-key isearch-mode-map [prior]
  'isearch-repeat-backward)
(global-set-key (kbd "C-<prior>") 'previous-buffer)	;; going to the previous buffer
(global-set-key (kbd "C-<next>") 'next-buffer)		;; going to the next buffer
(global-set-key (kbd "C-a") 'mark-whole-buffer)		;; Ctrl + a to select all
(global-set-key (kbd "C-<tab>") 'other-window)	        ;; for moving cursor between split-windows
(global-set-key (kbd "C-o") 'ido-find-file)		;; for opening a file
(global-set-key (kbd "C-<f4>") 'ido-kill-buffer)        ;; for closing a tab (browser like behaviour)
(global-set-key (kbd "M-f") 'menu-bar-open)             ;; for opening the file menu
(setq x-select-enable-clipboard t)                      ;; enabling copying and pasting between applications

;; indentation
(define-key global-map (kbd "RET") 'newline-and-indent) ;; indents on going to the newline
;; pasted lines are automatically indented
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

;; splitting windows
(global-set-key (kbd "C-|") 'split-window-horizontally)		;; for opening a file
(global-set-key (kbd "C--") 'split-window-vertically)		        ;; for opening a file

(ido-mode)
(show-paren-mode)
(diary)
(desktop-save-mode 1)
(color-theme-initialize)
(color-theme-midnight)
(tool-bar-mode)
(scroll-bar-mode)
(put 'upcase-region 'disabled nil)

;; auto-suggestions as we type
(set-default-font "-unknown-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
(add-to-list 'load-path "/home/ksinkar/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/ksinkar/.emacs.d//ac-dict")
(ac-config-default)
(put 'scroll-left 'disabled nil)

;; sets tabs to spaces for haml files
(add-hook 'haml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)
	     (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; haskell mode configuration
;; (setq auto-mode-alist
;;       (append auto-mode-alist
;;               '(("\\.[hg]s$"  . haskell-mode)
;;                 ("\\.hic?$"   . haskell-mode)
;;                 ("\\.hsc$"    . haskell-mode)
;;                 ("\\.chs$"    . haskell-mode)
;;                 ("\\.l[hg]s$" . literate-haskell-mode))))
;; (autoload 'haskell-mode "haskell-mode"
;;    "Major mode for editing Haskell scripts." t)
;; (autoload 'literate-haskell-mode "haskell-mode"
;;    "Major mode for editing literate Haskell scripts." t)

;; ;adding the following lines according to which modules you want to use:
;; (require 'inf-haskell)

;; (add-hook 'haskell-mode-hook 'turn-on-font-lock)
;; (add-hook 'haskell-mode-hook 'turn-off-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'turn-off-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;; (add-hook 'haskell-mode-hook 
;;    (function
;;     (lambda ()
;;       (setq haskell-program-name "ghci")
;;       (setq haskell-ghci-program-name "ghci6"))))

