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

(setq doom-font (font-spec :family "Monaco" :size 20))

;; Brighten the comments font color and set background to unspecified
;; https://www.w3schools.com/colors/colors_picker.asp
(custom-set-faces!
  '(font-lock-comment-face :slant italic :foreground "#848d94")
  '(font-lock-doc-face :slant italic :foreground "#848d94"))


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

(use-package! org-pandoc-import :after org)

;; (defun my/insert-date ()
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%d")))

;; (defun my/insert-time ()
;;   (interactive)
;;   (insert (format-time-string "%H:%M")))

;; (defun my/insert-date-time ()
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%d %H:%M")))

(use-package! company
  :config
  (setq company-idle-delay 2.5))

(use-package! company-box :after company
  :hook (company-mode . company-box-mode))

;; TODO: Make C-g exit company-mode completions before clearing the copilot overlay
;; or make a keybinding to switch between copilot and company completions. For now,
;; I'm just setting a delay on company completions.
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :hook (text-mode . copilot-mode)
  :hook (org-mode . copilot-mode)
  :bind* (
           :map copilot-completion-map
           ("<tab>"      . 'copilot-accept-completion)
           ("TAB"        . 'copilot-accept-completion)
           ("C-g"        . 'copilot-clear-overlay)
           ("<backtab>"  . 'copilot-accept-completion-by-word)
           ("M-<tab>"    . 'copilot-accept-completion-by-line)
           ("C-n"        . 'copilot-next-completion)
           ("C-p"        . 'copilot-previous-completion)
           ))

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
;; (use-package! hydra
;;   :defer
;;   :config
;;   (defhydra hydra/evil-window-resize (:color blue)
;;     "Resize window"
;;     ("h" evil-window-decrease-width "decrease width" :exit nil)
;;     ("j" evil-window-decrease-height "decrease height" :exit nil)
;;     ("k" evil-window-increase-height "increase height" :exit nil)
;;     ("l" evil-window-increase-width "increase width" :exit nil)
;;     ("q" nil "quit" :color red))
;;   (defhydra hydra/evil-window-move (:color blue)
;;     "Move window"
;;     ("h" evil-window-move-far-left "move far left" :exit nil)
;;     ("j" evil-window-move-very-bottom "move very bottom" :exit nil)
;;     ("k" evil-window-move-very-top "move very top" :exit nil)
;;     ("l" evil-window-move-far-right "move far right" :exit nil)
;;     ("q" nil "quit" :color red))
;;   (map! :leader
;;     :desc "Resize window"
;;     :n "w r" #'hydra/evil-window-resize/body
;;     :desc "Move window"
;;     :n "w m" #'hydra/evil-window-move/body))

(turn-on-auto-fill)

;; TODO: Make Emacs K search append "emacs" to search query

;; https://emacs.stackexchange.com/questions/47878/how-can-i-disable-a-specific-lint-error-for-emacs-lisp-using-flycheck
(add-hook 'emacs-lisp-mode-hook #'elisp-noflycheck-hook)
(defun elisp-noflycheck-hook ()
  "Add the ;;;###noflycheck thing to elisp."
  (require 'flycheck)
  (add-hook 'flycheck-process-error-functions #'flycheck-elisp-noflycheck nil t))
(defun flycheck-elisp-noflycheck (err)
  "Ignore flycheck if line of ERR ends with (flycheck-elisp-noflycheck-marker)."
  (save-excursion
    (goto-char (cdr (flycheck-error-region-for-mode err 'symbols)))
    (looking-back flycheck-elisp-noflycheck-marker
      (max (- (point) (length flycheck-elisp-noflycheck-marker))
        (point-min)))))
(defcustom flycheck-elisp-noflycheck-marker ";noflycheck"
  "Flycheck line regions marked with this comment are ignored."
  :type 'string
  :group 'flycheck)

;; TODO: Make flycheck-list-errors switch focus to the error buffer

;; TODO: Map C-o to winner-undo but only for popup-mode major modes overriding the
;; default binding of C-o to better-jumper-jump-backward.

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

;; (if 'my/using-emacs-plus-p
;;   (progn
;;     (message "Using emacs-plus")
;;     (setq ns-use-native-fullscreen t)
;;     ;; Retrieves either 'light' or 'dark' from MacOS system preferences
;;     ;; and sends it as an parameter to my/apply-theme
;;     (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
;;     ;; Enable pixel scrolling
;;     ;; (good-scroll-mode 1)
;;     )
;;   ;; Else
;;   (progn
;;     (setq doom-theme 'doom-one)))

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

;; (defun my/kill-whole-line-safely ()
;;   "Delete whole line except for unmatched parentheses and put them at the end
;; of the previous line."
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (sp-kill-sexp)
;;     (delete-indentation)))

;; TODO: Copilot copilot-accept-completion-by-word incorrectly places the cursor
;; at the end of the copilot overlay instead of at the beginning.

;; (defun my/evil-line-or-visual-line ()
;;   "Select whole line except for unmatched parentheses and include the line break
;; from the end of the previous line."
;;   (interactive)
;;   (save-excursion
;;     ;; (evil-previous-line)
;;     ;; (end-of-line)
;;     (beginning-of-line)
;;     (evil-visual-state 1)
;;     (sp-forward-sexp)))

;; (use-package! evil
  ;; :config
  ;; (advice-add 'evil-line-or-visual-line :override #'my/evil-line-or-visual-line))

;; (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

;; (defun my/comment-root-sexp-of-line-safely ()
;;   "Comment out the root sexp of the current line until the last closing
;; parenthesis of the root sexp."
;;   (interactive)
;;   (require 'paredit)
;;   (save-excursion
;;     (beginning-of-line)
;;     (mark-sexp)
;;     (paredit-comment-dwim)))

;; (defun my/open-below-inside-sexp ()
;;   "Open a new line below the current line but within the current sexp and enter
;; insert mode."
;;   (interactive)
;;   (beginning-of-line)
;;   (forward-sexp)
;;   (when (featurep 'paredit)
;;     (paredit-RET))
;;   (when (featurep 'evil)
;;     (evil-insert-state)))

;; TODO: Map dd to my/kill-whole-line

;; TODO: Automatic format-all-buffer

;; Enable +format-on-save-enabled-modes on save for all programming modes
;; (add-hook 'format-all-mode-hook
;;   (lambda ()
;;     (setq +format-on-save-enabled-modes ; TODO: not working
;;       '(not
;;          latex-mode
;;          tex-mode
;;          sql-mode
;;          ;; TODO: Troubleshoot copilot indentation not working right here.
;;          ))))

;; (add-hook 'flycheck-mode-hook #'my/disable-flycheck-for-emacs-d)
;; (defun my/disable-flycheck-for-emacs-d ()
;;   "Disable flycheck for files in ~/.emacs.d."
;;   (when (string-match-p "/Users/ryan/.emacs.d/" (buffer-file-name))
;;     (flycheck-mode nil)))

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

(global-hi-lock-mode 't)

(setq comment-auto-fill-only-comments t)

;; Disable git style convention checks' overlong summary line warning but keep red font in magit
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
