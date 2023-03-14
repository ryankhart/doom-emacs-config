;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Fullscreen mode options on startup
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
;; (add-hook 'window-setup-hook #'toggle-frame-maximized)
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

(load! "private")

(setq evil-cross-lines t) ; evil-mode h and l wrap around lines

;; Set the default tab size to 2 spaces
(setq-default tab-width 2)
(setq tab-width 2)
(setq lisp-indent-offset 2)

(setq doom-font (font-spec :family "Monaco" :size 19))
(setq doom-font-increment 1)

;; Brighten the comments font color and set background to unspecified
;; https://www.w3schools.com/colors/colors_picker.asp
(custom-set-faces!
  '(font-lock-comment-face :slant italic :foreground "#848d94")
  '(font-lock-doc-face :slant italic :foreground "#848d94"))

(use-package! rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode))

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
  :n "w C-<left>" nil
  :n "w C-<right>" nil
  :n "w C-<up>" nil
  :n "w C-<down>" nil
  :n "w <left>" nil
  :n "w <right>" nil
  :n "w <up>" nil
  :n "w <down>" nil
  :n "s B" nil
  :n "b l" nil
  :n "X" nil
  )

(map!
  :n "C-x C-;" nil
  :n "C-x C-g" nil
  :n "C-x C-s" nil
  :n "C-x C-w" nil
  :n "C-x <left>" nil
  :n "C-x <right>" nil
  :i "S-<left>" nil ; prevented shift-left from selecting text leftwards
  :i "S-<right>" nil ; prevented shift-right from selecting text rightwards
  )

(map!
  :leader
  :prefix ("w o" . "other windows")
  :prefix ("m" . "major mode")
  :prefix ("m e" . "eval")
  :prefix ("m d" . "debug")
  :prefix ("m g" . "goto")
  :prefix ("T" . "tools")
  )

(map!
  :leader
  :desc "search all open buffers" :n "s b" #'search-all-open-buffers
  :desc "delete all other windows" :n "w o o" #'delete-other-windows
  :desc "delete other windows👈👉" :n "w o s" #'doom/window-maximize-horizontally
  :desc "delete other windows👇👆" :n "w o v" #'doom/window-maximize-vertically
  :desc "browse URL in browser" :n "g x" #'browse-url-default-macosx-browser
  )

(map!
  :n "s-h" #'evil-first-non-blank-of-visual-line
  :n "s-l" #'evil-end-of-visual-line
  :n "s-<" #'customize-variable)

(setq org-directory             "~/Dropbox/org/")
(setq org-default-notes-file    "~/Dropbox/org/refile.org")
(setq org-capture-templates
  '(("t" "Todo" entry (file "~/Dropbox/org/refile.org")
      "* TODO %?\n  %i%a\n")
     ("n" "Note" entry (file "~/Dropbox/org/refile.org")
       "* %? :NOTE: \n  %i%a\n")))

(defun my/org-update-link-description ()
  "Replace URL at point with org link with fetched title as description."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (when url
      (delete-region
        (car (bounds-of-thing-at-point 'url))
        (cdr (bounds-of-thing-at-point 'url)))
      (insert (format "[[%s][%s]]" url (my/org-get-title-from-url url))))))
(defun my/org-get-title-from-url (url)
  "Return the title of the page at URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "<title>\\(.*\\)</title>")
    (match-string 1)))

(setq display-line-numbers-type 'relative)

;; Automatic changes to config get saved to separate file instead of this one.
(setq-default custom-file (expand-file-name "custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; TODO: Extract minimum-window-height and percent-margin into defcustom
;; variables.
(defun my/define-scroll-margins ()
  "Set the scroll margins dynamically based on the current window height."
  (let ((minimum-window-height 14))
    (let ((percent-margin 0.1))
      (setq scroll-margin
        (if (<= (window-body-height) minimum-window-height)
          0
          (floor (* (window-body-height) (* percent-margin 2))))))))
(add-hook 'window-configuration-change-hook #'my/define-scroll-margins)

(defun my/scroll-up-line (&optional ARG)
  "Scroll up ARG lines but keep point in the same place relative to the window."
  (interactive "p")
  (let ((arg (or ARG 1)))
    (unless (= (point-min) (window-start))
      (evil-previous-line arg)
      (scroll-down arg))))

(defun my/scroll-down-line (&optional ARG)
  "Scroll up ARG lines but keep point in the same place relative to the window."
  (interactive "p")
  (let ((arg (or ARG 1)))
    (evil-next-line arg)
    (scroll-up arg)))

(defun my/scroll-left (&optional ARG)
  "Scroll left ARG columns but keep point in the same place relative to the
window."
  (interactive)
  (let ((arg (or ARG 1)))
    (when (> (window-hscroll) 0)
      (progn
        (evil-backward-char arg)
        (scroll-right arg)))))

(defun my/scroll-right (&optional ARG)
  "Scroll right ARG columns but keep point in the same place relative to the
window."
  (interactive)
  (let ((arg (or ARG 1)))
    (and (scroll-left arg)
      (evil-forward-char arg))))

(display-time-mode 1)
(display-battery-mode 1)

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(defun my/comment-line-and-move-down ()
  "JetBrains style comment toggling"
  (interactive)
  (comment-line 1))
(map! :desc "Toggle comment line and move down one line"
  :n "s-/" #'my/comment-line-and-move-down)

;; TODO Add format on paste

;; TODO make goto definition open in existing buffer if it's already showing in
;; a visible window.

;; TODO include TODO comments from anywhere in the current project in my agenda

(use-package! org-pandoc-import :after org)

(defun my/insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun my/insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun my/insert-date-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(use-package! company
  :config
  (setq company-idle-delay 2.0))

(use-package! company-box :after company
  :hook (company-mode . company-box-mode))

;; TODO: Make C-g exit company-mode completions before clearing the copilot
;; overlay or make a keybinding to switch between copilot and company
;; completions. For now, I'm just setting a delay on company completions.
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :hook (text-mode . copilot-mode)
  :hook (org-mode . copilot-mode)
  :bind* (:map copilot-completion-map
           ("<tab>"      . 'copilot-accept-completion)
           ("TAB"        . 'copilot-accept-completion)
           ("C-g"        . 'copilot-clear-overlay)
           ("<backtab>"  . 'copilot-accept-completion-by-word)
           ("M-<tab>"    . 'copilot-accept-completion-by-line)
           ("C-n"        . 'copilot-next-completion)
           ("C-p"        . 'copilot-previous-completion)))

;; TODO use add-advice :before to make my custom evil open function work with
;; elisp

(use-package! format-all
  :config
  (setq +format-on-save-enabled-modes ; Enable for emacs-lisp-mode
    (cl-delete 'emacs-lisp-mode +format-on-save-enabled-modes :test #'eq))
  (add-hook! 'evil-paste-after-hook       #'format-all-buffer)
  (add-hook! 'yank-hook                   #'format-all-buffer)
  (defvar evil-paste-after-hook nil
    "Hook run after pasting.")
  (defvar yank-hook nil
    "Hook run after yanking.")
  (defadvice! evil-paste-after-hook (&rest _)
    "Run `evil-paste-after-hook' after evil pasting."
    :after #'evil-paste-after
    (run-hooks 'evil-paste-after-hook))
  (defadvice! yank-hook (&rest _)
    "Run `yank-hook' after Emacs yanking (pasting)."
    :after #'yank
    (run-hooks 'yank-hook)))

(use-package! aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "white")
  :bind
  ("C-;" . iedit-mode))

(if (fboundp 'format-all-mode)
  (add-hook 'prog-mode-hook #'format-all-mode))

(defun my/save-buffer ()
  (interactive)
  (evil-normal-state)
  (if (buffer-modified-p)
    (save-buffer)
    (message "File is already saved")))
(map! :desc "Save File"
  "s-s" #'my/save-buffer)
(map! :leader
  :desc "Save File"
  :n "f s" #'my/save-buffer)

;; TODO: Very low priority, but find out why :exit isn't nil by default
;; like it says in the docs.
(use-package! hydra
  ;; :defer
  :config
  (defhydra +hydra/window-nav (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _u_:undo  _r_:redo
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
         Resize: _H_:left  _J_:down  _K_:up  _L_:right
           Move: _h_:left  _k_:up  _j_:down  _l_:right  _i_menu
"
    ("h" (my/scroll-left 2))
    ("j" (my/scroll-down-line 1))
    ("k" (my/scroll-up-line 1))
    ("l" (my/scroll-right 2))
    ("i" idomenu)

    ("u" winner-undo)
    ("r" winner-redo)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" switch-to-buffer)
    ("f" find-file)

    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" (hydra-move-splitter-left 2))
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" (hydra-move-splitter-right 2))

    ("q" nil))

  (map! :leader
    ;; :desc "Resize window"
    ;; :n "w r" #'hydra/evil-window-resize/body
    ;; :desc "Move window"
    ;; :n "w m" #'hydra/evil-window-move/body
    :desc "Window Navigation"
    :n "SPC" #'+hydra/window-nav/body
    :desc "Modify Hex Color"
    :n "T m" #'+rgb/kurecolor-hydra/body))

;; TODO Make the hydra window nav leave the point where it was and let it go out
;; of the window.

;; TODO Make prepending comments that cause the line to be too long
;; automatically wrap to the next line. between the ;; and the rest of the
;; comment.  Use "g w a c" for "wrap around comment"

(turn-on-auto-fill)

;; TODO: Make Emacs K search append "emacs" to search query

;; TODO: Map C-o to winner-undo but only for popup-mode major modes overriding
;; the default binding of C-o to better-jumper-jump-backward.

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-solarized-light t))
    ('dark (load-theme 'doom-one t))))

(defun my/using-emacs-plus-p ()
  "Return t if using emacs-plus, nil otherwise."
  (string-match-p "emacs-plus" (car command-line-args)))

(defun my/using-emacs-mac-p ()
  "Return t if using emacs-mac, nil otherwise.
TODO: Untested as I do not use emacs-mac currently"
  (string-match-p "emacs-mac" (car command-line-args)))

(if 'my/using-emacs-plus-p
  (progn
    (message "Using emacs-plus")
    (setq ns-use-native-fullscreen t)
    ;; Retrieves either 'light' or 'dark' from MacOS system preferences
    ;; and sends it as an parameter to my/apply-theme
    (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
    ;; Enable pixel scrolling
    ;; (good-scroll-mode 1)
    )
  ;; Else
  (progn
    (setq doom-theme 'doom-one)))

(if 'my/using-emacs-mac-p
  (progn
    (message "Using emacs-mac")
    ;; Enable pixel scrolling
    (setq mac-mouse-wheel-smooth-scroll 't)
    ;; Retrieves either 'light' or 'dark' from MacOS system preferences
    ;; and sends it as an parameter to my/apply-theme
    )
  ;; Else
  (progn))

(defun my/kill-whole-line-safely ()
  "Delete whole line except for unmatched parentheses and put them at the end
of the previous line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (sp-kill-sexp)
    (delete-indentation)))

(defun my/evil-line-or-visual-line ()
  "Select whole line except for unmatched parentheses and include the line break
from the end of the previous line."
  (interactive)
  (save-excursion
    ;; (evil-previous-line)
    ;; (end-of-line)
    (beginning-of-line)
    (evil-visual-state 1)
    (sp-forward-sexp)))

(defun my/comment-root-sexp-of-line-safely ()
  "Comment out the root sexp of the current line until the last closing
parenthesis of the root sexp."
  (interactive)
  (require 'paredit)
  (save-excursion
    (beginning-of-line)
    (mark-sexp)
    (paredit-comment-dwim)))

(defun my/open-below-inside-sexp ()
  "Open a new line below the current line but within the current sexp and enter
insert mode."
  (interactive)
  (beginning-of-line)
  (forward-sexp)
  (when (featurep 'paredit)
    (paredit-RET))
  (when (featurep 'evil)
    (evil-insert-state)))

;; TODO: Map dd to my/kill-whole-line

;; TODO: Automatic format-all-buffer

;; TODO: Make Copilot keep the cursor at the beginning of the overlay

;; TODO: https://discourse.doomemacs.org/t/flymake-for-private-config/3515

;; TODO: Find out why apropos is not working

;; (command-log-mode)

(setq custom-buffer-done-kill 't)

;; TODO: Make Customize buffer open in with a full frame

;; ;; Set a custom function for evil motion 'd d' to my/kill-whole-line-safely
;; (evil-define-motion evil-delete-whole-line (count)
;;   "Delete whole line except for unmatched parentheses and put them at the end
;; of the previous line."
;;   :type line
;;   (my/kill-whole-line-safely))

(setq global-hi-lock-mode t)

(setq comment-auto-fill-only-comments t)

;; Disable git style convention checks' overlong summary line warning but keep
;; red font in magit
(setq git-commit-style-convention-checks '(non-empty-second-line))

;; Set which-key max height by percent of screen (default is 1.0)
(setq max-mini-window-height 0.5)
(setq package-selected-packages '(iedit))
(setq show-paren-delay 0)
(setq show-paren-mode t)
(setq warning-minimum-level :error)

(advice-add #'magit-blame-quit
  :after (lambda (&rest _)
           (message "Exiting magit-blame mode")))

(defun my/toggle-window-split-direction ()
  "Toggle the split direction of the current window."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
            (next-win-buffer (window-buffer (next-window)))
            (this-win-edges (window-edges (selected-window)))
            (next-win-edges (window-edges (next-window)))
            (this-win-2nd (not (and (<= (car this-win-edges)
                                      (car next-win-edges))
                                 (<= (cadr this-win-edges)
                                   (cadr next-win-edges)))))
            (splitter
              (if (= (car this-win-edges)
                    (car (window-edges (next-window))))
                'split-window-horizontally
                'split-window-vertically)))
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1)))
      (window-split-toggle))
    (message "This function only works with two windows.")))
(map! :leader
  :desc "Toggle win split direction" "t s" #'my/toggle-window-split-direction)

(use-package! flycheck
  :defer t
  :config
  (setq flycheck-mode nil)
  (setq flycheck-check-syntax-automatically '(save idle-change))
  ;; TODO: Make flycheck-list-errors switch focus to the error buffer

;; https://google.com
(use-package! w3m
  :init
  (setq browse-url-browser-function 'w3m-browse-url)
  )

(use-package! evil
  :init
  (setq evil-respect-visual-line-mode t)
  (map! :n "j" #'evil-next-visual-line
    :n "k" #'evil-previous-visual-line))

;; Stop messages like these from appearing in the echo area when I run a
;; command. I already can see the keymap from the M-x menu.
;; "You can run the command ‘count-words’ with g C-g"
(setq suggest-key-bindings nil)

(advice-add #'evil-window-split
  :after #'prompt-for-buffer-in-other-window)
(advice-add #'evil-window-vsplit
  :after #'prompt-for-buffer-in-other-window)
(defun prompt-for-buffer-in-other-window (&rest _)
  "Prompt for a buffer to open in the other window.
If the user cancels the prompt, the other window is deleted."
  (other-window 1)
  (condition-case nil
    (call-interactively #'consult-buffer)
    (quit (delete-window))))

(map!
  :n "s-j" #'evil-scroll-down
  :n "s-k" #'evil-scroll-up)
