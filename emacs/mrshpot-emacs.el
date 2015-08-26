;;; Global customizations
(require 'cl)

(defalias 'qrr 'query-replace-regexp)
(defalias 'rs 'replace-string)

(global-set-key [(control return)] 'calculator)
(global-set-key (kbd "C-c <C-return>") 'redraw-display)

(setq backup-by-copying t)

;; MinGW/MSYS shell
(if (not (null (getenv "MSYSTEM"))) ; are we under MSYS?
	(setq explicit-shell-file-name "bash")
	(setq shell-file-name (concat exec-directory "cmdproxy.exe")))

;; Indent on C-j
; (electric-indent-mode 0)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-m") 'electric-newline-and-maybe-indent)

;; override whatever the default is
(prefer-coding-system 'utf-8)

;; goodbye, splash screen
(setq inhibit-splash-screen t)

;; nice editing features
(global-hl-line-mode t)
(show-paren-mode t)
(setq transient-mark-mode t)

;; mode-line stuff
(line-number-mode 1)
(column-number-mode 1)
(display-time-mode t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; turn off gui stuff I don't use anyway
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; nicer frame titles
(setq frame-title-format
	  (concat "%b - "
			  (or (getenv "USER")
				  (getenv "USERNAME"))
			  "@" (system-name)))



;;; Set up packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
			 '("marmalade" . "http://marmalade-repo.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defconst *mrshpot-packages*
  (list 'zenburn-theme
		'browse-kill-ring 'smooth-scrolling 'smart-mode-line
		'lua-mode 'markdown-mode 'cmake-mode
		'cuda-mode 'csharp-mode
		'pkgbuild-mode)
  "Packages that should be installed")
(let
    ((query-package-install
      (lambda (name)
	(when (and (not (package-installed-p name))
		   (yes-or-no-p (format "Install %s? " name)))
	  (package-install name)))))
  (mapcar query-package-install *mrshpot-packages*))


(require 'smooth-scrolling)

;;; Miscellaneous tweaks

;; Theme
(load-theme 'zenburn)

;; automatically reload externally changed files
(global-auto-revert-mode t)

;; automatically handle compressed files
(auto-compression-mode t)

;; default to unified diffs
(setq diff-switches "-u")

;; ediff
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; TODO: Diminish -- remove or rename modelines for minor modes
; (require 'diminish)

;; CamelCase navigation
(global-subword-mode 1)

;; always end a file with a newline
(setq require-final-newline 'query)

;; elisp, emacs lisp
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(setq mode-name "el")))

;; shell
;; Fix shell-mode color special characters
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; shell customizations
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 ; '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 ; '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

;; move between windows with M-arrows
(windmove-default-keybindings 'meta)

;; cc-mode
;; my preferred indentation style
(setq c-default-style "ellemtel")
(setq c-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

;; tabs
;; force 4-space tabs
(setq tab-width 4)
(setq default-tab-width 4)
(setq indent-tabs-mode nil)
(add-hook 'c-mode-common-hook (lambda ()
				(setq tab-width 4)
				(setq indent-tabs-mode nil)))

;; IDO, Interactively Do Things
;; for nicer autocompletion in minibuffer
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)

;; dired
(defvar dired-buffer-prefix "dired-"
  "Prefix for the dired advice `dired-add-prefix'")

(defadvice dired (after dired-add-prefix activate)
  "Prefix the dired buffer name with `dired-buffer-prefix' if it's non-nil"
  (when dired-buffer-prefix
	(with-current-buffer ad-return-value
	  (unless (string-match
			   (concat "^" dired-buffer-prefix ".*")
			   (buffer-name))
		(rename-buffer (concat dired-buffer-prefix (buffer-name)))))))

(put 'dired-find-alternate-file 'disabled t)
; hide group in dired listings
(setq dired-listing-switches "-alGh")

; for dired-omit-mode
(require 'dired-x)

;; Python
(add-to-list 'auto-mode-alist '("\\.pyw$" . python-mode))

;; DocView
;; automatically switch to the next page
(eval-after-load 'doc-view '(setq doc-view-continuous t))

;; Uniquify files with same name in different directories
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; CUDA
(setq auto-mode-alist (append '(("/*.\.cuh$" . cuda-mode)) auto-mode-alist))

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; ElDoc mode -- display documentation about symbol under cursor
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; browse-kill-ring
(require 'browse-kill-ring)
; (require 'browse-kill-ring+)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; CMake
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
(add-to-list 'auto-mode-alist '("/*.\.cmake$" . cmake-mode))

;; Arch Linux PKGBUILD mode
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.\\(text\\|md\\)" . markdown-mode))

;; TODO: sml/smart mode line
(sml/setup)
(sml/apply-theme 'respectful)

;; fold-dwim
(global-set-key (kbd "<f7>")      'fold-dwim-toggle)
(global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
(global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)

;; Always ask before killing Emacs
;; Set as the last thing to decrease annoyance on buggy init files
(setq confirm-kill-emacs #'yes-or-no-p)

;; optional stuff that may or may not be present, as I don't use that for work anyway
(cl-labels
    ((optional-require (feature)
		       (or (require feature nil t)
			   (progn (message "Could not load optional feature %s. Skipping." feature) nil))))
  ;; EMMS
  (when (optional-require 'emms-setup)
    (emms-standard)
    (emms-default-players)
    (load "my-emms"))

  ;; W3M pager browser
  (when (optional-require 'w3m)
    (setq browse-url-browser-function 'w3m-browse-url))

  ;; StarDict Console Version
  (when (optional-require 'sdcv-mode)
    (global-set-key (kbd "C-c d") 'sdcv-search)))

(load "mrshpot-helpers.el")



;;; More Unicode stuff
(require 'iso-transl)
(iso-transl-define-keys
 `(("^0" . ,(vector (decode-char 'ucs #x2070)))
   ("^4" . ,(vector (decode-char 'ucs #x2074))) ; 1-3 already defined
   ("^5" . ,(vector (decode-char 'ucs #x2075)))
   ("^6" . ,(vector (decode-char 'ucs #x2076)))
   ("^7" . ,(vector (decode-char 'ucs #x2077)))
   ("^8" . ,(vector (decode-char 'ucs #x2078)))
   ("^9" . ,(vector (decode-char 'ucs #x2079)))
   ("^+" . ,(vector (decode-char 'ucs #x207A)))
   ("^-" . ,(vector (decode-char 'ucs #x207B)))
   ("^=" . ,(vector (decode-char 'ucs #x207C)))
   ("^(" . ,(vector (decode-char 'ucs #x207D)))
   ("^)" . ,(vector (decode-char 'ucs #x207E)))
   ("_0" . ,(vector (decode-char 'ucs #x2080)))
   ("_1" . ,(vector (decode-char 'ucs #x2081)))
   ("_2" . ,(vector (decode-char 'ucs #x2082)))
   ("_3" . ,(vector (decode-char 'ucs #x2083)))
   ("_4" . ,(vector (decode-char 'ucs #x2084)))
   ("_5" . ,(vector (decode-char 'ucs #x2085)))
   ("_6" . ,(vector (decode-char 'ucs #x2086)))
   ("_7" . ,(vector (decode-char 'ucs #x2087)))
   ("_8" . ,(vector (decode-char 'ucs #x2088)))
   ("_9" . ,(vector (decode-char 'ucs #x2089)))
   ("_+" . ,(vector (decode-char 'ucs #x208A)))
   ("_-" . ,(vector (decode-char 'ucs #x208B)))
   ("_=" . ,(vector (decode-char 'ucs #x208C)))
   ("_(" . ,(vector (decode-char 'ucs #x208D)))
   ("_)" . ,(vector (decode-char 'ucs #x208E)))))
