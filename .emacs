;; before you use this .emacs
;; Please follow the following instructions:-
;; If you are on Ubuntu please run the following commands
;; # apt-get install emacs-goodies-el
;; for enabling haml mode
;; # apt-get install haml-elisp 
;; If you are on Fedora please run the following commands
;; # yum install emacs-color-theme
;; For auto-complete to work properly, download the package from the following site and install it using the instructions given on the site
;; http://cx4a.org/software/auto-complete/
;; For auto-parenthesis to work properly, download the package .el file from the following site and put in the load path -> ~/.emacs.d/
;; http://code.google.com/p/autopair/
;; To enable diary, create an empty file named diary in your home directory

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
(global-set-key (kbd "C-<f4>") 'kill-this-buffer)       ;; for closing a tab (browser like behaviour)
(global-set-key (kbd "M-f") 'menu-bar-open)             ;; for opening the file menu
(setq x-select-enable-clipboard t)                      ;; enabling copying and pasting between applications

;; indentation
(define-key global-map (kbd "RET") 'reindent-then-newline-and-indent) ;; indents on going to the newline
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
						     html-mode       css-mode
						     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

;; splitting windows
(global-set-key (kbd "C-|") 'split-window-horizontally)		;; for splitting a window horizontally
(global-set-key (kbd "C--") 'split-window-vertically)		;; for splitting a window vertically

(ido-mode)			;; enabling listing of file names
(show-paren-mode)		;; enabling highlighting of parenthesis
(diary)				;; enabling diary mode
(desktop-save-mode 1)		;; enabling desktop save mode
(tool-bar-mode 0)		;; disabling the toolbar
(scroll-bar-mode 0)		;; disabling the scrollbar
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;(global-linum-mode 1)           ;; enabling seeing line numbers in the l.h.s. buffer margin

(set-default-font "-unknown-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1") ;; setting the default font to ubuntu mono

;; loading the emacs load path from where the extension libraries are loaded
(add-to-list 'load-path "/home/ksinkar/.emacs.d/")

;; color-themes
(require 'color-theme)
(color-theme-initialize)	;; initializing color themes
(color-theme-midnight)		;; using the midnight color theme

;; auto-suggestions as we type
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/ksinkar/.emacs.d//ac-dict")
(ac-config-default)

;; simulating the electric-pair-mode, auto parenthesis
(require 'autopair)
(autopair-global-mode)                                     ;; enable autopair in all buffers

;; sets tabs to spaces for haml files
(add-hook 'haml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)
	     (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; opens rake files in ruby mode
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))

;; opens less style files in css mode
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))

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

