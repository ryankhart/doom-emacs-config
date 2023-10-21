;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Fullscreen mode on startup
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

(load! "private")

(setq evil-cross-lines t) ; evil-mode h and l wrap around lines

;; (setq-default tab-width 2)
;; (setq tab-width 2)
;; (setq lisp-indent-offset 2)

(setq doom-font (font-spec :family "Monaco" :size 19))
(setq doom-font-increment 1)

;; https://www.w3schools.com/colors/colors_picker.asp
;; Use SPC T m to modify hex color at point
(custom-set-faces!
  '(font-lock-comment-face :slant italic :foreground "#848d94")
  '(font-lock-doc-face :slant italic :foreground "#848d94"))

;; Change background color and forground color of show-paren-mode
(custom-set-faces!
  '(show-paren-match :background "#848d94" :foreground "#ffffff"))

;; Add advice to change the threshold for when rainbow-mode changes the
;; background color of a string to white.
(advice-add #'rainbow-color-luminance
            ;; Make foreground color white 10% sooner than the default
            :filter-return (lambda (luminance) (- luminance 0.1)))

;; rainbow-mode is a minor mode that sets background color to strings
;; that match color names, e.g. #0000ff is displayed in white with a blue background.
(use-package! rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode))

(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline t :background nil)

;; TODO: Make rainbow-mode background color take priority over hl-line-mode without
;; disabling hl-line-mode.

;; TODO: Separate all SPC h describe bindings into a separate prefix to avoid
;; multiple pages of bindings in the help menu.

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
 :n "w c" nil
 :n "w :" nil
 :n "w f" nil
 ;; set all "w C-<key>" to nil
 :n "w C-_" nil
 :n "w C-f" nil
 :n "w C-q" nil
 :n "w C-r" nil
 :n "w C-u" nil
 :n "w C-b" nil
 :n "w C-c" nil
 :n "w C-n" nil
 :n "w C-o" nil
 :n "w C-p" nil
 :n "w C-s" nil
 :n "w C-t" nil
 :n "w C-v" nil
 :n "w C-w" nil
 :n "w C-x" nil
 :n "w C-y" nil
 :n "w C-z" nil
 :n "w C-SPC" nil
 :n "w C-<backspace>" nil
 :n "w C-<delete>" nil
 :n "w C-<down>" nil
 :n "w C-<end>" nil
 :n "w C-<home>" nil

 ;; Set all "w C-S-<key>" to nil
 :n "w C-S-<backspace>" nil
 :n "w C-S-<delete>" nil
 :n "w C-S-<down>" nil
 :n "w C-S-<end>" nil
 :n "w C-S-<home>" nil
 :n "w C-S-<left>" nil
 :n "w C-S-<right>" nil
 :n "w C-S-<up>" nil
 :n "w C-S-b" nil
 :n "w C-S-c" nil
 :n "w C-S-d" nil
 :n "w C-S-f" nil
 :n "w C-S-h" nil
 :n "w C-S-j" nil
 :n "w C-S-k" nil
 :n "w C-S-l" nil
 :n "w C-S-n" nil
 :n "w C-S-o" nil
 :n "w C-S-p" nil
 :n "w C-S-q" nil
 :n "w C-S-r" nil
 :n "w C-S-s" nil
 :n "w C-S-t" nil
 :n "w C-S-u" nil
 :n "w C-S-v" nil
 :n "w C-S-w" nil
 :n "w C-S-x" nil
 :n "w C-S-y" nil
 :n "w C-S-z" nil
 :n "w C-S-SPC" nil
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
 :n "b k" nil
 :n "b K" nil
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
 :desc "Kill all buffers" :n "b D" #'doom/kill-all-buffers
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

(use-package! bag
  :config
  ;; TODO: Set keybinding for bag-lap and bag-replace-all
  ;; TODO: Test and experiment with bag-tags
  (setq bag-tags
        '("gnu"
          ("editors" ("emacs" "vi"))
          ("dev" (nil "lisp" "python")))))

;; TODO: Replace this with bag.el from Github
;; (defun my/org-update-link-description ()
;;   "Replace URL at point with org link with fetched title as description."
;;   (interactive)
;;   (let ((url (thing-at-point 'url)))
;;     (when url
;;       (delete-region
;;         (car (bounds-of-thing-at-point 'url))
;;         (cdr (bounds-of-thing-at-point 'url)))
;;       (insert (format "[[%s][%s]]" url (my/org-get-title-from-url url))))))
;; (defun my/org-get-title-from-url (url)
;;   "Return the title of the page at URL."
;;   (with-current-buffer (url-retrieve-synchronously url)
;;     (goto-char (point-min))
;;     (re-search-forward "<title>\\(.*\\)</title>")
;;     (match-string 1)))

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
  ;; Set keybinding for +company/complete
  :bind ("C-<tab>" . #'my/company-complete-popup)
  :config
  (setq company-idle-delay nil))

(defun my/company-complete-popup ()
  "Call +company/complete and then call copilot-clear-overlay."
  (interactive)
  (call-interactively #'+company/complete)
  (call-interactively #'copilot-clear-overlay))

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

;; (use-package! format-all
;;   :config
;;   (setq +format-on-save-enabled-modes ; Enable for emacs-lisp-mode
;;     (cl-delete 'emacs-lisp-mode +format-on-save-enabled-modes :test #'eq))
;;   (add-hook! 'evil-paste-after-hook       #'format-all-buffer)
;;   (add-hook! 'yank-hook                   #'format-all-buffer)
;;   (defvar evil-paste-after-hook nil
;;     "Hook run after pasting.")
;;   (defvar yank-hook nil
;;     "Hook run after yanking.")
;;   (defadvice! evil-paste-after-hook (&rest _)
;;     "Run `evil-paste-after-hook' after evil pasting."
;;     :after #'evil-paste-after
;;     (run-hooks 'evil-paste-after-hook))
;;   (defadvice! yank-hook (&rest _)
;;     "Run `yank-hook' after Emacs yanking (pasting)."
;;     :after #'yank
;;     (run-hooks 'yank-hook)))

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
      (pixel-scroll-precision-mode 1) ; Emacs 29+ only
      )
  (progn
    (setq doom-theme 'doom-one)))

(defun my/replace-elisp-docstrings (replacement)
  "Replace all Elisp docstrings in the current buffer with REPLACEMENT."
  (interactive "sReplacement: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*\"\"\"\\(.*\\)\"\"\"" nil t)
      (replace-match replacement))))

(when 'my/using-emacs-mac-p
  (progn
    (message "Using emacs-mac")
    (setq mac-mouse-wheel-smooth-scroll 't)))

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

;; TODO: Map C-o to winner-undo but only for popup-mode major modes overriding
;; the default binding of C-o to better-jumper-jump-backward.
(add-hook! 'popup-mode-hook
  (define-key popup-mode-map (kbd "C-o") 'winner-undo))

;; When in evil-insert-state and +company/complete is active, pressing escape will
;; exit company-mode, remain in evil-insert-state, and not clear any copilot overlay.
(map! :map company-active-map
      "ESC" #'company-abort
      [escape] #'company-abort)

;; While inn evil-insert-state, disable show-paren-mode temporarily. Then re-enable
;; it when exiting evil-insert-state.
(add-hook! 'evil-insert-state-entry-hook
  (setq show-paren-mode nil))
(add-hook! 'evil-insert-state-exit-hook
  (setq show-paren-mode t))

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
  ;; When flycheck-list-errors is called move cursor to the first error
  (advice-add #'flycheck-list-errors
              :after (lambda (&rest _)
                       (flycheck-first-error))))

(use-package! markdown-mode
  :defer t
  ;; Make `markdown-live-preview-mode' open in a vertical split
  :config
  ;; TODO unknown if this is working or not
  (defadvice! markdown-live-preview-mode--open-in-vertical-split (_ &rest _)
    :after #'markdown-live-preview-mode
    (my/toggle-window-split-direction)))

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

(use-package! org-ai
  :init
  (org-ai-global-mode))

;; Stop messages like these from appearing in the echo area when I run a
;; command. I already can see the keymap from the M-x menu.
;; "You can run the command ‘count-words’ with g C-g"
(setq suggest-key-bindings nil)

;; FIXME: my/buffers-with-no-file-to-save-to-p always returns non-nil.
;; (defadvice! doom--warn-on-unsaved-buffers-a (orig-fn &rest _)
;;   "Warn me if I try to quit emacs with buffers that have yet to be saved ever."
;;   :before #'doom-quit-p
;;   (when (my/buffers-with-no-file-to-save-to-p)
;;     (user-error "Aborting: There are unsaved buffers that cannot be autosaved")))
(defun my/buffers-with-no-file-to-save-to-p ()
  "Return non-nil if there are unsaved buffers do not have a file to save to."
  (cl-loop for buf in (buffer-list)
           if (and (buffer-modified-p buf)
                   (not (buffer-file-name buf)))
           return t))
;; TODO Fix bug where it doesnt let me quit emacs on the home screen

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

;; TODO Replace all setq with setq!

;; TODO Make auto intenting work when closing a parenthesis

(require 'org-mouse)
