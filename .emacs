;; Avoid garbage collection during startup. Helps with perf
(setq gc-cons-threshold most-positive-fixnum)

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Removing weird gaps when resized
(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

;; Remove emacs startup landing page
(setq inhibit-startup-message t)

;; Check if each of these modes exists before trying to disable them
(when (fboundp 'scroll-bar-mode) ; Disable visual scroll bar
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) ; Disable tool bar
  (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) ; Disable tooltips
  (tooltip-mode -1))
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode 10))
(when (fboundp 'menu-bar-mode) ; Disable menubar
  (menu-bar-mode -1))
(setq ring-bell-function 'ignore) ; Turn off alarms completely

(let ((backup-dir "~/.emacs.d/backup/")
      (auto-saves-dir "~/.emacs.d/auto-saves/"))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir))
  (unless (file-exists-p auto-saves-dir)
    (make-directory auto-saves-dir))
  
  ;; Make backups of files, even when they're in version control
  (setq vc-make-backup-files t)

  ;; backup settings
  (setq backup-directory-alist `(("." . ,backup-dir))
        backup-by-copying t    ; Don't delink hardlinks
        version-control t      ; Use version numbers on backups
        delete-old-versions t  ; Automatically delete excess backups
        kept-new-versions 20   ; How many of the newest versions to keep
        kept-old-versions 5)   ; And how many of the old

  ;; auto-save settings
  (setq auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-default t
        delete-by-moving-to-trash t
        auto-save-timeout 20
        auto-save-interval 200))

(when (fboundp 'column-number-mode)
  (column-number-mode))

(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Install and configure ivy and counsel
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(setq ivy-initial-inputs-alist nil)

;; Use different regex strategies per command
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))

(use-package counsel
  :ensure t
  :bind (("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package all-the-icons
  :init
  (unless (or (package-installed-p 'all-the-icons)
              (file-exists-p (expand-file-name "~/.local/share/fonts/NFM.ttf")))
    (package-refresh-contents)
    (package-install 'all-the-icons)
    ;; Install the font files if they aren't present
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-persp-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding t)
  :hook
  (after-init . doom-modeline-mode))

(if (version< emacs-version "26.1")
    (if (require 'nlinum nil 'noerror)
        (progn
          (global-nlinum-mode t)
          (setq nlinum-format "%4d \u2502 "))
      (progn
        (require 'linum)
        (global-linum-mode t)
        (setq linum-format "%4d \u2502 ")))
  (use-package display-line-numbers
    :config
    (global-display-line-numbers-mode)
    (set-face-attribute 'line-number nil :foreground "#5C6773")
    (set-face-attribute 'line-number-current-line nil :foreground "#5C6773")))

;; ================
;; Customization
;; ================

;; Check if the system is capable of handling font changes
(when (find-font (font-spec :name "Iosevka Nerd Font"))
  (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 120))

(let* ((class '((class color) (min-colors 89)))
       ;; Palette
       (palette '(
                  (bg        . "#000000")
                  (comment   . "#5C6773")
                  (markup    . "#F07178")
                  (constant  . "#FFEE99")
                  (operator  . "#E7C547")
                  (tag       . "#36A3D9")
                  (regexp    . "#95E6CB")
                  (string    . "#a19959")
                  (function  . "#FFB454")
                  (special   . "#E6B673")
                  (keyword   . "#FF7733")
                  (error     . "#FF3333")
                  (accent    . "#F29718")
                  (panel     . "#14191F")
                  (guide     . "#2D3640")
                  (line      . "#151A1E")
                  (selection . "#253340")
                  (fg        . "#E6E1CF")
                  (fg-idle   . "#3E4B59")))

       (bg        (cdr (assoc 'bg        palette)))
       (fg        (cdr (assoc 'fg        palette)))
       (comment   (cdr (assoc 'comment   palette)))
       (constant  (cdr (assoc 'constant  palette)))
       (function  (cdr (assoc 'function  palette)))
       (keyword   (cdr (assoc 'keyword   palette)))
       (string    (cdr (assoc 'string    palette)))
       (error     (cdr (assoc 'error     palette)))
       (selection (cdr (assoc 'selection palette)))
       (accent    (cdr (assoc 'accent    palette)))
       (panel     (cdr (assoc 'panel     palette)))
       (line      (cdr (assoc 'line      palette))))

  ;; Apply the color theme to faces
  (custom-theme-set-faces
   'user
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(font-lock-builtin-face ((,class (:foreground ,constant))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,function))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(region ((,class (:background ,selection))))
   `(vertical-border ((,class (:foreground ,panel))))
   `(minibuffer-prompt ((,class (:foreground ,keyword))))
   `(font-lock-warning-face ((,class (:foreground ,error))))
   `(mode-line ((,class (:background ,line :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,line :foreground ,fg)))))

  (set-face-background 'fringe bg))

;; Reset garbage collection.
(setq gc-cons-threshold 16777216)

(provide 'init)
;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-modeline all-the-icons counsel ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:background "#000000" :foreground "#E6E1CF"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#FFEE99"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "#5C6773"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#FFB454"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#FF7733"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#a19959"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#E6E1CF"))))
 '(font-lock-warning-face ((((class color) (min-colors 89)) (:foreground "#FF3333"))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#FF7733"))))
 '(mode-line ((((class color) (min-colors 89)) (:background "#151A1E" :foreground "#E6E1CF"))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:background "#151A1E" :foreground "#E6E1CF"))))
 '(region ((((class color) (min-colors 89)) (:background "#253340"))))
 '(vertical-border ((((class color) (min-colors 89)) (:foreground "#14191F")))))
