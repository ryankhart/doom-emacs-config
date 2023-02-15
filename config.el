;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "private")

(setq doom-theme 'doom-one)

;; Make evil-mode h and l wrap around lines
(setq evil-cross-lines t)

;; Make Emacs act like a proper editor
(setq debug-on-error t)

;; Set the default tab size to 2 spaces
(setq-default tab-width 2)

;; Turn off Emacs quit confirmation
;; I get enough quit confirmation prompts from unsaved files
(setq confirm-kill-emacs nil)

;; Brighten the comments font color
(custom-set-faces!
  '(font-lock-comment-face :slant italic :foreground "#5B6268"))

(map!
 :leader
 :n "w C-h" nil
 :n "w C-l" nil
 :n "w C-k" nil
 :n "w C-j" nil
 :n "w q" nil
 :n "w R" nil
 :n "w m" nil
 :n "w o" nil
 :n "s B" nil
 )

(map!
 :leader
 :prefix ("w o" . "other windows")
 :prefix ("m" . "major mode")
 :prefix ("m e" . "eval")
 :prefix ("m d" . "debug")
 :prefix ("m g" . "goto")
 )

(map!
 :leader
 :desc "search all open buffers" :n "s b" #'search-all-open-buffers
 :desc "delete all other windows" :n "w o o" #'delete-other-windows
 :desc "delete other windows👈👉" :n "w o s" #'doom/window-maximize-horizontally
 :desc "delete other windows👇👆" :n "w o v" #'doom/window-maximize-vertically
 )

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
    ;; Percent margin per top/bottom for windows larger than
    ;; minimum-window-height
    (let ((percent-margin 0.1))
      (with-demoted-errors
          (let ((computed-margin
                 (if (<= (window-body-height) minimum-window-height)
                     0
                   (floor (* (window-body-height) (/ percent-margin) 2) ))))
            (setq scroll-margin computed-margin))))))
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
  ;; When using company-mode, enable company-box-mode
  :hook (company-mode . company-box-mode))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind* (("<backtab>" . 'copilot-accept-completion-by-word)
          :map copilot-completion-map
          ("<tab>" . 'copilot-accept-completion)
          ("TAB" . 'copilot-accept-completion))
  :config
  ;; Upon entering copilot mode, disable smartparens-mode as it is incompatible
  (add-hook 'copilot-mode-hook #'turn-off-smartparens-mode)
  ;; Only enable copilot in insert mode and immediately after entering insert mode
  (setq copilot-enable-predicates '(evil-insert-state-p)))

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

(use-package! command-log-mode
  :defer
  :config
  (command-log-mode 1))

;; If buffer can be formatted...
(if (fboundp 'format-all-mode)
    ;; ...enable format-all-mode to auto format on save
    ;; so that you don't get message from format-all-mode
    ;; when it is not available
    (add-hook 'prog-mode-hook #'format-all-mode))

(defun my/save-buffer ()
  (interactive)
  (if (buffer-modified-p)
      (save-buffer)
    (message "File is already saved")))
(map! :desc "Save File"
      "s-s" #'my/save-buffer)
(map! :leader
      :desc "Save File"
      :n "f s" #'my/save-buffer)

(use-package! hydra
  :defer
  :config
  (defhydra hydra/evil-window-resize (:color blue)
    "Resize window"
    ("h" evil-window-decrease-width "decrease width")
    ("j" evil-window-decrease-height "decrease height")
    ("k" evil-window-increase-height "increase height")
    ("l" evil-window-increase-width "increase width")
    ("q" nil "quit" :color red))
  (defhydra hydra/evil-window-move (:color blue)
    "Move window"
    ("h" evil-window-move-far-left "move far left")
    ("j" evil-window-move-very-bottom "move very bottom")
    ("k" evil-window-move-very-top "move very top")
    ("l" evil-window-move-far-right "move far right")
    ("q" nil "quit" :color red))
  (map!
   :leader
   :desc "window resize" :n "w r" #'hydra/evil-window-resize/body
   :desc "window move" :n "w m" #'hydra/evil-window-move/body))

;; Make evil-delete-whole-line delete whole line except for any unmatched
;; closing parentheses and move all closing parentheses at the end of the
;; current line to the end of the previous line
(defun my/evil-delete-whole-line ()
  (interactive)
  (let ((line (thing-at-point 'line)))
    (evil-delete-whole-line (point-at-bol) (point-at-eol))
    ;; Count the number of unmatched closing parentheses at the end of the line
    ;; TODO: Fix bug where "(" or ")" inside a string is counted when it shouldn't.
    (let ((unmatched-closing-parentheses-count
           (- (length (split-string line ")")) (length (split-string line "(")))))
      ;; If there are unmatched closing parentheses, move them to the end of the
      ;; previous line and move the cursor to the end of the previous line
      ;; Otherwise, move the cursor to the beginning of the current line
      (if (> unmatched-closing-parentheses-count 0)
          (progn
            (evil-previous-line)
            (end-of-line)
            (insert (s-repeat unmatched-closing-parentheses-count ")")))
        (beginning-of-line)))))
(setq-default fill-column 80)
(turn-on-auto-fill)

