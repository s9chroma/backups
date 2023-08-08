;; Avoid garbage collection during startup. Helps with perf
(setq gc-cons-threshold most-positive-fixnum)

;; System specific paths
(defvar my-dart-flutter-sdk-path "/home/pavan/Software/flutter")
(defvar my-dart-sdk-path (concat my-dart-flutter-sdk-path "/bin/cache/dart-sdk"))

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Scroll gradually
(setq scroll-conservatively 101)
(setq scroll-margin 3)
(setq scroll-step 1)

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

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Work")
    (setq projectile-project-search-path '("~/Work")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

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
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))

(defun ivy-open-in-vertical-split (file)
  "Open FILE in a vertical split."
  (split-window-right)
  (other-window 1)
  (find-file file))

(ivy-add-actions
 'counsel-find-file
 '(("v" ivy-open-in-vertical-split "open in vertical split")
   ("j" find-file-other-window "open in other window")))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config)

;; Have to manually initialize. Putting this in (use-package) causes bugs
(ivy-mode 1)
(counsel-mode 1) 

(use-package ivy-rich
             :init
             (ivy-rich-mode 1))

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
  :custom ((doom-modeline-height 55)))

(setq all-the-icons-scale-factor 1.1)

;; LSP Mode Configuration
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((dart-mode . lsp-deferred))
  :init
  (setq lsp-enable-snippet nil)  ; Disable snippets as Dart LSP doesn't support it by default
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-diagnostics-provider :auto) ; Use :flycheck if you have flycheck installed and want to use it
  ;; Use the custom variables for paths
  (setq lsp-dart-flutter-sdk-dir my-dart-flutter-sdk-path)
  (setq lsp-dart-sdk-dir my-dart-sdk-path))

;; LSP UI Configuration
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-diagnostic-position 'top))

(define-key evil-normal-state-map (kbd "C-<right>") 'flymake-goto-next-error)
(define-key evil-normal-state-map (kbd "C-<left>") 'flymake-goto-prev-error)

;; LSP Dart Configuration
(use-package lsp-dart
  :ensure t
  :after lsp-mode
  :config)

;; Dont show external dialogs
(setq use-dialog-box nil)

;; Company Configuration
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)  ; How long to wait before starting auto-completion
  (setq company-minimum-prefix-length 0)  ; Start autocompletion after 2 characters
  (setq company-selection-wrap-around t)  ; Continue from top when reaching the bottom and vice versa
  (global-company-mode t)
  ;; Keybindings for cycling through completions
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

;; Ensure LSP uses company-capf
(setq lsp-prefer-capf t)

(defvar lsp-doc-shown nil "Whether the LSP documentation is currently shown.")

(defun toggle-lsp-doc ()
  "Toggle the display of LSP documentation."
  (interactive)
  (if lsp-doc-shown
      (progn
        (lsp-ui-doc-hide)
        (setq lsp-doc-shown nil))
    (progn
      (lsp-ui-doc-glance)
      (setq lsp-doc-shown t))))

(add-hook 'lsp-mode-hook 'lsp-ui-doc-mode)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") 'toggle-lsp-doc))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-undo-system 'undo-tree)
  :bind
  (:map evil-normal-state-map
        ("u" . undo-tree-undo)
        ("C-r" . undo-tree-redo)))

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo-tree-hist/"))))
  (global-undo-tree-mode 1)
  :config
  (evil-define-key 'normal 'global (kbd "C-r") 'undo-tree-redo)
  (evil-define-key 'normal 'global (kbd "u") 'undo-tree-undo)
  (evil-define-key 'normal 'global (kbd "C-x u") 'undo-tree-visualize))

;; Custom function to find references in another window with a horizontal split
(defun lsp-find-references-horizontal-split ()
  "Find references in another window with a horizontal split."
  (interactive)
  (let ((origin-buffer (current-buffer))
        (split-height-threshold 0) ; Force horizontal split
        (split-width-threshold nil))
    (lsp-find-references)
    (when (not (eq origin-buffer (current-buffer)))
      (switch-to-buffer origin-buffer)
      (let ((new-window (split-window-below)))
        (select-window new-window)
        (switch-to-buffer (other-buffer))))))

;; Custom function to find references in another window with a vertical split
(defun lsp-find-references-vertical-split ()
  "Find references in another window with a vertical split."
  (interactive)
  (let ((origin-buffer (current-buffer))
        (split-width-threshold 0)) ; Force vertical split
    (lsp-find-references)
    (when (not (eq origin-buffer (current-buffer)))
      (switch-to-buffer origin-buffer)
      (let ((new-window (split-window-right)))
        (select-window new-window)
        (switch-to-buffer (other-buffer))))))

(defvar lsp-jump-back-marker nil
  "Marker for jumping back after jumping to a definition.")

(defun lsp-toggle-jump-definition ()
  "Toggle between jumping to a definition and jumping back."
  (interactive)
  (if lsp-jump-back-marker
      (progn
        (let ((buf (marker-buffer lsp-jump-back-marker))
              (pos (marker-position lsp-jump-back-marker)))
          (if (and buf pos)
              (progn
                (switch-to-buffer buf)
                (goto-char pos))
            (message "Marker is not valid anymore.")))
        (setq lsp-jump-back-marker nil))
    (setq lsp-jump-back-marker (point-marker))
    (lsp-find-definition)))

(defun lsp-find-definition-same-window ()
  "Find definition in the current window."
  (interactive)
  (lsp-find-definition)
  (other-window -1)) ;; Focus the current window even if the definition is in another file

(defun lsp-find-definition-other-window ()
  "Find definition in another window with a vertical split."
  (interactive)
  ;; Create a vertical split if there's only one window
  (when (one-window-p t)
    (split-window-right))
  ;; Invoke the LSP find definition in the other window
  (let ((current-window (selected-window)))
    (other-window 1)
    (lsp-find-definition)
    ;; If the definition didn't switch buffers, return to the original window
    (when (eq current-window (selected-window))
      (other-window -1))))

(defun lsp-find-references-horizontal-split ()
  "Find references and display them in a horizontal split. If a split already exists, just display in the other window."
  (interactive)
  ;; Check if there's more than one window in the current frame
  (unless (> (count-windows) 1)
    ;; If not, split the window horizontally
    (split-window-below))
  ;; Finally, call the lsp-find-references function
  (lsp-find-references))

(defun lsp-find-references-vertical-split ()
  "Find references and display them in a vertical split. If a split already exists, just display in the other window."
  (interactive)
  ;; Check if there's more than one window in the current frame
  (unless (> (length (window-list)) 1)
    ;; If not, split the window vertically
    (split-window-right))
  ;; Finally, call the lsp-find-references function
  (lsp-find-references))

(defvar global-lsp-toggle-jump-last-position nil
  "Holds the last position before jumping to a definition. Used to toggle back.")

(defun lsp-toggle-jump-definition ()
  "Toggle between the current location and the definition."
  (interactive)
  ;; If we have a stored position, jump back to it
  (if global-lsp-toggle-jump-last-position
      (let ((last-pos global-lsp-toggle-jump-last-position))
        (setq global-lsp-toggle-jump-last-position nil)
        (switch-to-buffer (car last-pos))
        (goto-char (cdr last-pos)))
    ;; Otherwise, store the current buffer and position, and jump to the definition
    (progn
      (setq global-lsp-toggle-jump-last-position (cons (current-buffer) (point)))
      (lsp-find-definition))))

(with-eval-after-load 'dart-mode
  (evil-define-key 'normal dart-mode-map (kbd "gd") 'lsp-find-definition-same-window)
  (evil-define-key 'normal dart-mode-map (kbd "gD") 'lsp-find-definition-other-window)
  (evil-define-key 'normal dart-mode-map (kbd "gr") 'lsp-find-references-horizontal-split)
  (evil-define-key 'normal dart-mode-map (kbd "gR") 'lsp-find-references-vertical-split)
  (evil-define-key 'normal dart-mode-map (kbd "gz") 'lsp-toggle-jump-definition))

(defun my-dart-format-after-save ()
  "Use `dart format` from the specified SDK path to format the current buffer after saving."
  (when (eq major-mode 'dart-mode) ; Only run for Dart files
    (let* ((dart-bin-path (concat my-dart-sdk-path "/bin"))
           (dart-format-command (concat dart-bin-path "/dart format"))
           (filename (shell-quote-argument (buffer-file-name))))
      (message "Formatting Dart file with dart format...")
      (shell-command (concat dart-format-command " " filename " -o write"))
      ;; Reload the file contents after formatting
      (revert-buffer nil t))))

(add-hook 'after-save-hook 'my-dart-format-after-save)

(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode 1))

(use-package evil-commentary)
(evil-commentary-mode)

;; Install and configure yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(with-eval-after-load 'yasnippet
  (define-key evil-insert-state-map (kbd "M-o") 'yas-expand))

;; Define a function to ensure the Doom snippets repo is cloned
(defun ensure-doom-snippets-cloned ()
  (let ((doom-snippets-dir (expand-file-name "~/.emacs.d/doom-snippets")))
    (unless (file-exists-p doom-snippets-dir)
      (message "Doom snippets not found. Cloning now...")
      (shell-command (concat "git clone https://github.com/doomemacs/snippets.git " doom-snippets-dir))
      (message "Doom snippets cloned successfully!"))))

;; Call the function to ensure the repo is cloned
(ensure-doom-snippets-cloned)

;; Load the Doom snippets
(use-package doom-snippets
  :load-path "~/.emacs.d/doom-snippets"
  :after yasnippet)

;; auto-closing-pairs
(electric-pair-mode 1)

(defun should-display-line-numbers-p ()
  "Determine if line numbers should be displayed for the current buffer."
  (and (not (derived-mode-p 'messages-buffer-mode
                            'help-mode
                            'shell-mode
                            'eshell-mode
                            'term-mode
			    'org-mode
                            ;; Add any other modes here
                            ))
       (not (minibufferp))))

;; Modify the line number activation based on Emacs version
(if (version< emacs-version "26.1")
    (if (require 'nlinum nil 'noerror)
        (progn
          (add-hook 'after-change-major-mode-hook
                    (lambda ()
                      (when (should-display-line-numbers-p)
                        (nlinum-mode 1))))
          (setq nlinum-format "%4d \\u2502 "))
      (progn
        (require 'linum)
        (add-hook 'after-change-major-mode-hook
                  (lambda ()
                    (when (should-display-line-numbers-p)
                      (linum-mode 1))))
        (setq linum-format "%4d \\u2502 ")))
  (use-package display-line-numbers
    :config
    (add-hook 'after-change-major-mode-hook
              (lambda ()
                (when (should-display-line-numbers-p)
                  (display-line-numbers-mode 1))))
    (set-face-attribute 'line-number nil :foreground "#5C6773")
    (set-face-attribute 'line-number-current-line nil :foreground "#5C6773")))

;; Start server for faster subsequent emacs startups
(require 'server)
(unless (server-running-p)
    (server-start))

;; ================
;; Keybindings
;; ================

;; Split keybinds
(defun evil-window-vsplit-and-focus ()
  "Vsplit and move to the new window."
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1))

(defun evil-window-split-and-focus ()
  "Split and move to the new window."
  (interactive)
  (evil-window-split)
  (evil-window-down 1))

(define-key evil-normal-state-map (kbd "SPC v") 'evil-window-vsplit-and-focus)
(define-key evil-normal-state-map (kbd "SPC h") 'evil-window-split-and-focus)
(define-key evil-normal-state-map (kbd "C-J") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-K") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-L") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-H") 'evil-window-left)

;; Config file settings
(defun open-user-init-file ()
  "Open the user's init file."
  (interactive)
  (find-file user-init-file))

(define-key evil-normal-state-map (kbd "SPC rc") 'open-user-init-file)

(defun reload-config ()
  "Reload the user's Emacs init file."
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded successfully!"))

(define-key evil-normal-state-map (kbd "SPC rv") 'reload-config)

;; Buffer delete
(defun kill-current-buffer-no-prompt ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

;; Find file
(define-key evil-normal-state-map (kbd "SPC f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC g") (lambda () (interactive) (counsel-find-file default-directory)))
(with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC F") 'projectile-switch-project))
(define-key evil-normal-state-map (kbd "SPC b") 'counsel-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC a") 'counsel-rg)

(defun open-file-in-manager ()
  "Open the directory of the current file in the file manager based on the OS."
  (interactive)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory)))
    (cond
     ((string-equal system-type "windows-nt") ; Microsoft Windows
      (start-process "explorer" nil "explorer" dir))
     ((string-equal system-type "darwin") ; macOS
      (start-process "finder" nil "open" dir))
     ((string-equal system-type "gnu/linux") ; Linux
      (start-process "file-manager" nil "xdg-open" dir)))))

;; Define the keybinding
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC e") 'open-file-in-manager))

;; General keybinds
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC c") 'evil-scroll-line-to-center)

(defun close-window-or-quit-emacs ()
  "Close the current window. If it's the last window, quit Emacs."
  (interactive)
  (if (one-window-p)
      (save-buffers-kill-terminal)
    (delete-window)))

(define-key evil-normal-state-map (kbd "SPC c") 'close-window-or-quit-emacs)
(define-key evil-normal-state-map (kbd "SPC q") 'kill-current-buffer-no-prompt)

(defun close-and-kill ()
  "Kill the current buffer without a prompt, then close the current window or quit Emacs."
  (interactive)
  (kill-current-buffer-no-prompt)
  (close-window-or-quit-emacs))

(define-key evil-normal-state-map (kbd "SPC x") 'close-and-kill)

(defun immortal-scratch-buffer ()
  "Prevent the *scratch* buffer from being killed."
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "The *scratch* buffer cannot be killed.")
    nil))

(add-hook 'kill-buffer-query-functions 'immortal-scratch-buffer)

;; Install required packages if not already done
;; (use-package is used as an example; adjust according to your package manager)
(use-package all-the-icons :ensure t)

;; Better bullets for Org-mode
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Set TODO keywords with different colors and icons
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff6347" :weight bold))
        ("IN-PROGRESS" . (:foreground "#ffa500" :weight bold))
        ("WAITING" . (:foreground "#ffd700" :weight bold))
        ("DONE" . (:foreground "#32cd32" :weight bold))))

(setq org-agenda-files '("~/org/")) ;; adjust this path to your Org files
(setq org-agenda-window-setup 'current-window)

;; ================
;; Customization
;; ================
;; Check if the system is capable of handling font changes
(when (find-font (font-spec :name "Iosevka Nerd Font"))
    (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 125 :weight 'normal))

;; Define the color palette
(setq custom-palette
      '((bg . "#0A0A0A")
        (comment . "#5C6773")
        (markup . "#F07178")
        (constant . "#FFEE99")
        (operator . "#E7C547")
        (tag . "#36A3D9")
        (regexp . "#95E6CB")
        (string . "#86B300")
        (function . "#FFB454")
        (special . "#E6B673")
        (keyword . "#FF7733")
        (error . "#FF3333")
        (accent . "#F29718")
        (panel . "#14191F")
        (guide . "#2D3640")
        (line . "#151A1E")
        (selection . "#253340")
        (fg . "#E6E1CF")
        (fg-idle . "#3E4B59")))

;; Apply the color palette to the corresponding Emacs faces
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-ayu-mirage t))
(set-face-background 'fringe (cdr (assoc 'bg custom-palette)))
(set-face-background 'default (cdr (assoc 'bg custom-palette)))
(set-face-foreground 'default (cdr (assoc 'fg custom-palette)))
(set-face-foreground 'font-lock-comment-face (cdr (assoc 'comment custom-palette)))
(set-face-foreground 'font-lock-function-name-face (cdr (assoc 'function custom-palette)))
(set-face-foreground 'font-lock-keyword-face (cdr (assoc 'keyword custom-palette)))
(set-face-foreground 'font-lock-constant-face (cdr (assoc 'constant custom-palette)))
(set-face-foreground 'font-lock-string-face (cdr (assoc 'string custom-palette)))
(set-face-foreground 'font-lock-type-face (cdr (assoc 'special custom-palette)))
(set-face-foreground 'font-lock-variable-name-face (cdr (assoc 'markup custom-palette)))
(set-face-foreground 'font-lock-builtin-face (cdr (assoc 'operator custom-palette)))
(set-face-background 'region (cdr (assoc 'selection custom-palette)))
(set-face-foreground 'error (cdr (assoc 'error custom-palette)))
(set-face-background 'mode-line (cdr (assoc 'line custom-palette)))
(set-face-background 'mode-line-inactive (cdr (assoc 'guide custom-palette)))
(set-face-foreground 'font-lock-regexp-grouping-construct (cdr (assoc 'regexp custom-palette)))
(set-face-foreground 'font-lock-regexp-grouping-backslash (cdr (assoc 'regexp custom-palette)))
(set-face-background 'highlight (cdr (assoc 'accent custom-palette)))
(set-face-background 'vertical-border (cdr (assoc 'line custom-palette)))
(set-face-foreground 'minibuffer-prompt (cdr (assoc 'accent custom-palette)))
(set-face-foreground 'font-lock-preprocessor-face (cdr (assoc 'operator custom-palette)))
(set-face-background 'secondary-selection (cdr (assoc 'selection custom-palette)))
(set-face-background 'isearch (cdr (assoc 'regexp custom-palette)))
(set-face-foreground 'lazy-highlight (cdr (assoc 'fg-idle custom-palette)))
(set-face-background 'lazy-highlight (cdr (assoc 'selection custom-palette)))

(set-face-foreground 'doom-modeline-buffer-path (cdr (assoc 'accent custom-palette)))
(set-face-foreground 'doom-modeline-buffer-major-mode (cdr (assoc 'fg custom-palette)))
(set-face-foreground 'doom-modeline-buffer-minor-mode (cdr (assoc 'fg-idle custom-palette)))
(set-face-background 'doom-modeline-bar (cdr (assoc 'keyword custom-palette)))
(set-face-background 'doom-modeline-highlight (cdr (assoc 'accent custom-palette)))
(set-face-foreground 'doom-modeline-info (cdr (assoc 'function custom-palette)))
(set-face-foreground 'doom-modeline-warning (cdr (assoc 'keyword custom-palette)))
(set-face-foreground 'doom-modeline-urgent (cdr (assoc 'error custom-palette)))
(set-face-foreground 'doom-modeline-evil-insert-state (cdr (assoc 'string custom-palette)))
(set-face-foreground 'doom-modeline-evil-visual-state (cdr (assoc 'regexp custom-palette)))
(set-face-foreground 'doom-modeline-evil-normal-state (cdr (assoc 'comment custom-palette)))
(set-face-foreground 'doom-modeline-buffer-file (cdr (assoc 'function custom-palette)))
(set-face-foreground 'doom-modeline-buffer-modified (cdr (assoc 'function custom-palette)))

;; Company mode faces
(set-face-background 'company-tooltip (cdr (assoc 'panel custom-palette)))
(set-face-foreground 'company-tooltip (cdr (assoc 'fg custom-palette)))
(set-face-background 'company-tooltip-selection (cdr (assoc 'selection custom-palette)))
(set-face-foreground 'company-tooltip-selection (cdr (assoc 'fg custom-palette)))
(set-face-background 'company-scrollbar-bg (cdr (assoc 'guide custom-palette)))
(set-face-background 'company-scrollbar-fg (cdr (assoc 'comment custom-palette)))
(set-face-background 'company-tooltip-common (cdr (assoc 'bg custom-palette)))
(set-face-foreground 'company-tooltip-common (cdr (assoc 'keyword custom-palette)))
(set-face-background 'company-tooltip-common-selection (cdr (assoc 'selection custom-palette)))
(set-face-foreground 'company-tooltip-common-selection (cdr (assoc 'keyword custom-palette)))
(set-face-foreground 'company-tooltip-annotation (cdr (assoc 'comment custom-palette)))
(set-face-foreground 'company-tooltip-search (cdr (assoc 'accent custom-palette)))
(set-face-foreground 'company-tooltip-search-selection (cdr (assoc 'accent custom-palette)))


(set-fontset-font t 'unicode (font-spec :file "/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf") nil 'prepend)

;; ================
;; Org Mode Configuration
;; ================
(defun my-org-mode-company-settings ()
  (setq-local company-backends '((company-dabbrev company-files))))

(add-hook 'org-mode-hook 'my-org-mode-company-settings)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; Reset gc-hack at top
(setq gc-cons-threshold 2000000)
