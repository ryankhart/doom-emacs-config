;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

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

(setq display-line-numbers-type `relative)
(setq display-line-numbers-width 3)

;; Set minimum line padding between cursor and top and bottom of window
(setq scroll-margin 10)

;; Set up highlighting matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Customize parentheses highlighting to be more visible
(set-face-attribute 'show-paren-match nil
                    :foreground "#000000"
                    :weight 'extra-bold)

;; Set the show-paren-match background color to be bright orange
(set-face-background 'show-paren-match "#ffffff")
; Enable pixel scrolling for MacOS trackpad
(setq mac-mouse-wheel-smooth-scroll 't)

;; Fullscreen mode on startup
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; Here are some additional functions/macros that could help you configure Doom:
;;
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

(use-package! org-pandoc-import :after org)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion))
  ;; Enable childframe option in company module ((company +childframe)) to prevent overlay conflict
        :config (setq copilot-company-completion-backend 'company-childframe))
