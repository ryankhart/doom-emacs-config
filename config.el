;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Hart"
      user-mail-address "ryankhart@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Make evil-mode h and l wrap around lines
(setq evil-cross-lines t)

;; Brighten the comments font color
(custom-set-faces!
  '(font-lock-comment-face :slant italic :foreground "#5B6268"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; Set up org-mode to use the agenda files in the org directory
(setq org-agenda-files (list "~/Dropbox/org/agenda/"))

;; Set up org-mode to use the refile.org file as the inbox
(setq org-default-notes-file "~/Dropbox/org/refile.org")

;; Set up org-mode to use the todo.org file as the todo list
(setq org-todo-file "~/Dropbox/org/todo.org")

;; Set up org-mode to use the archive.org file as the archive
(setq org-archive-file "~/Dropbox/org/archive.org")

;; Set up org-mode to use the personal.org file as the personal file
(setq org-personal-file "~/Dropbox/org/personal.org")

;; Org-mode recommended file structure:
;; ~/Dropbox/org/
;;  ├── archive.org
;;  ├── refile.org  ;; <- your inbox
;;  ├── todo.org
;;  ├── personal.org
;;      ├── journal
;;      ├── habits
;;      ├── finance
;;      ├── health
;;      ├── home
;;      ├── travel
;;      ├── wishlist
;;      ├── ideas
;;      ├── recipes
;;  ├── content.org
;;    ├── reading
;;    ├── movies
;;    ├── music
;;    ├── podcasts
;;    ├── videos
;;    ├── games
;;    ├── software
;;    ├── tools
;;    ├── tv
;;  ├── notes.org
;;  ├── tools.org

(setq display-line-numbers-mode t)
(setq display-line-numbers-type `relative)
(setq display-line-numbers-width 3)

;; Ensure that any automatic changes to config get saved in separate file
(setq-default custom-file (expand-file-name "custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Configure fonts
(set-face-attribute 'default nil :font "Monaco 16")

(defun my/define-scroll-margins (&rest _args)
  "Set the scroll margins dynamically based on the current window height."
  (interactive)
  ;; if the window is too small, don't add any margins
  (let ((minimum-window-height 14))
    ;; Percent margin per top/bottom for windows larger than minimum-window-height
    (let ((percent-margin 0.1))

      (with-demoted-errors
          (let ((computed-margin
                 (if (<= (window-body-height) minimum-window-height)
                     0
                   (floor (* (window-body-height) (/ percent-margin) 2) ))))
            (setq
             scroll-margin computed-margin))))))

(add-hook 'post-command-hook #'my/define-scroll-margins)

;; Set up highlighting matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Enable indentation guides (vertical lines to show indentation)
;; for all programming modes
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Enable pixel scrolling for MacOS trackpad
(setq mac-mouse-wheel-smooth-scroll 't)

;; Mode line settings
(display-time-mode 1)
(display-battery-mode 1)

;; Fullscreen mode on startup
;; Each method has slightly different effects that vary from OS to OS. You’ll
;; have to decide for yourself which you prefer.
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
;; (add-hook 'window-setup-hook #'toggle-frame-maximized)
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Use idle-highlight-mode in every buffer
(add-hook 'prog-mode-hook #'idle-highlight-mode)

;; Add keymap for Command-/ to toggle a comment line and also move the cursor down one line
(map! :desc "Toggle comment line and move down one line"
      :n "s-/" #'my/comment-line-and-move-down)

(defun my/comment-line-and-move-down ()
  (interactive)
  (comment-line 1))

(use-package! org-pandoc-import :after org)

(use-package! company-box
  :hook (company-mode . company-box-mode)) ; When using company-mode, enable company-box-mode


;; Disable TAB and <tab> keymaps for company-mode
(map! :after company
      :map company-active-map
      "TAB" nil
      "<tab>" nil)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode) ; enable copilot in all programming modes
  ;; :bind* overrides all minor mode bindings (unlike :bind)
  :bind* (:map copilot-completion-map
               ;; Emacs usually translates <tab> to TAB, but we map both just
               ;; in case one of them was bound to different commands on the global-map
               ("C-TAB" . 'copilot-accept-completion-by-word)
               ("C-<tab>" . 'copilot-accept-completion-by-word)
               ("<tab>" . 'copilot-accept-completion)
               ("TAB" . 'copilot-accept-completion)))

(defun my/completion-tab ()
  (interactive)
    (or (copilot-accept-completion)
        (or (company-indent-or-complete-common nil) (indent-for-tab-command))))

(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "white")
  :bind
  ("C-;" . iedit-mode))

(map! :n "s-h" #'evil-first-non-blank-of-visual-line
      :n "s-l" #'evil-end-of-visual-line)

;; Disable git style convention checks' overlong summary line warning but keep red font in magit
;; Default options are: (non-empty-second-line overlong-summary-line)
(setq git-commit-style-convention-checks '(non-empty-second-line))

;; Enable command-log-mode
(use-package! command-log-mode
  :defer
  :config
  (global-command-log-mode 1)
  (setq command-log-mode-auto-show t))

(map! :leader
      :prefix ("w" . "window")
      :n "o" #'delete-other-windows)
