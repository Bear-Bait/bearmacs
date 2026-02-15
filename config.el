;;; $DOOMDIR/config-macos.el -*- lexical-binding: t; -*-
;;
;; macOS-specific Doom Emacs configuration
;; This config removes Linux-specific components (EXWM, dmenu, rofi, X11 tools)
;; and uses environment variables for API keys instead of hardcoding them.
;;
;; Setup Instructions:
;; 1. Copy .env.example to .env
;; 2. Fill in your API keys in .env
;; 3. The .env file will be automatically loaded on startup
;; 4. NEVER commit .env to version control!

;; Load environment variables from .env file
(defun load-env-file (file)
  "Load environment variables from FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
          (when (and (not (string-match-p "^#" line))
                    (not (string-match-p "^[[:space:]]*$" line))
                    (string-match "^\\([^=]+\\)=\\(.*\\)$" line))
            (let ((key (match-string 1 line))
                  (value (match-string 2 line)))
              (setenv key value))))
        (forward-line 1)))))

;; Load .env file from doom directory
(load-env-file (expand-file-name ".env" doom-user-dir))

;; Helper function to get environment variables with fallback
(defun get-env-or-warn (var-name &optional default)
  "Get environment variable VAR-NAME or return DEFAULT.
Warn user if variable is not set and no default provided."
  (or (getenv var-name)
      default
      (progn
        (warn "Environment variable %s not set! Please check your .env file." var-name)
        "")))


;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family ES fr Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Monaspace Xenon" :size 14)
      doom-variable-pitch-font (font-spec :family "Monaspace Neon" :size 16)
      doom-serif-font (font-spec :family "Monaspace Xenon" :size 16))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-material)

;; Weather Report - David Lynch style weather
(load! "weather-report")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/cloud/org/")

(after! emacsql
	(setq emacsql-sqlite-executable "/usr/bin/sqlite3"))
;;; Configure lockfiles to prevent sync conflicts
(setq create-lockfiles t)  ; Enable lockfiles globally

;; Set lockfile naming pattern
(setq lock-file-name-transforms
     '(("\\`/.*/\\([^/]+\\)\\'" "/tmp/\\1" t)))

;; Files that should never be locked
(setq create-lockfiles-ignore-patterns
     '("\\.#.+$"
        "\\.git/.*"
        "\\.tmp$"
        "^/tmp/.*"
        "/node_modules/.*"))
;;
;; Enable buffer locking to prevent accidental closing
(setq emacs-lock-default-locking-mode 'all)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;The display-buffer-alist configuration should handle *Messages* correctly. Ensure that the display-buffer-alist entry for *Messages* is processed by adding it to the list before EXWM is enabled.
;;(defun my-display-buffer-in-bottom-side-window (buffer alist)
  "Display BUFFER in a side window at the bottom with fixed size."
;;  (let ((window (display-buffer-in-side-window buffer alist)))
;;    (when window
;;      (with-selected-window window
;;        (setq window-size-fixed t))
;;      window)))

;; Configuration for displaying *Messages* buffer
;;(add-to-list 'display-buffer-alist
;;             '("*Messages*"
;;               (my-display-buffer-in-bottom-side-window)
;;               (side . bottom)
;;               (slot . 0)
;;               (window-height . 0.3)))
;;
;; (after! package
;;  (customize-set-variable 'package-archive-priorities
;;                         '(("melpa"  . 99)
;;                           ("gnu"    . 90)
;;                           ("nongnu" . 80))))
;; Vertico config
;;(after! vertico
;;  (setq vertico-cycle t)              ; cycle through candidates
;;  (setq vertico-count 13)             ; show 13 candidates
;;  (setq vertico-resize t))            ; resize minibuffer to fit candidates
;; Orderless config (for better fuzzy matching)
;;(after! orderless
;;  (setq completion-styles '(orderless basic)
;;        completion-category-defaults nil
;;       completion-category-overrides '((file (styles . (partial-completion))))))
;;
;;
;;Corfu config
;; Corfu config
(after! corfu
  (setq corfu-auto t                  ;; Enable auto completion
        corfu-auto-delay 0.2          ;; Delay before showing completions
        corfu-auto-prefix 2           ;; Show completions after typing 2 characters
        corfu-preview-current t       ;; Preview current selection
        corfu-cycle t                 ;; Enable cycling through candidates
        corfu-quit-at-boundary t      ;; Quit at completion boundary
        corfu-separator ?\                ;; Space as separator
        corfu-quit-no-match t)        ;; Quit if there's no match
  (global-corfu-mode))

;; Cape provides completion backend extensions
(use-package! cape
  :after corfu
  :init
  ;; Add Cape completion backends to the completion-at-point-functions list
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Language-specific completions
  (add-hook! 'python-mode-hook
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-keyword)))

  (add-hook! 'latex-mode-hook
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-tex)))

  (add-hook! 'html-mode-hook
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-sgml)))

  (add-hook! 'emacs-lisp-mode-hook
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-elisp)))

  (add-hook! 'org-mode-hook
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-elisp-block)
      (add-to-list 'completion-at-point-functions #'cape-keyword)
      (add-to-list 'completion-at-point-functions #'cape-symbol))))

;; Enable variable-pitch for writing modes
(add-hook 'text-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; Use TeX Gyre Pagella in elfeed
(add-hook 'elfeed-search-mode-hook #'variable-pitch-mode)
(add-hook 'elfeed-show-mode-hook #'variable-pitch-mode)

;; Additional Corfu-related packages
(use-package! corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.2 . 0.1)))

;; Configure completion styles - works great with Corfu
(after! corfu
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Useful key bindings for Corfu
(map! :map corfu-map
      "TAB" #'corfu-next
      "S-TAB" #'corfu-previous
      "C-n" #'corfu-next
      "C-p" #'corfu-previous
      "RET" #'corfu-insert
      "C-g" #'corfu-quit)

;; Dabbrev configuration for better prose completion
(use-package! dabbrev
  :config
  ;; Swap M-/ and C-M-/
  (global-set-key (kbd "M-/") 'dabbrev-completion)
  (global-set-key (kbd "C-M-/") 'dabbrev-expand)
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
;; Marginalia for rich annotations
;;(after! marginalia
;;  (marginalia-mode))

;; Helpful Embark bindings
;;(map! :after embark
;;      :map minibuffer-local-map
;;      "C-;" #'embark-act)      ; bind to whatever key you like
;;
;;GETTING THAT EM-DASH RIGHT ALT - - THIS FUCKING WORKED I AM SO FUCKNG PROUD OF YOU "ALT - -" is now '——' WHAATS UP
;; Define a compose key
(setq x-compose-key "M-")
;; Define the em dash sequence
(define-key key-translation-map (kbd "M--") (kbd "—"))
;;
;; )     ( /(   (  (      (  `
;; (     )\())  )\))(   ' )\))(
;; )\   ((_)\  ((_)()\ ) ((_)()\
;;((_)  __((_) _(())\_)()(_()((_)
;;| __| \ \/ / \ \((_)/ /|  \/  |
;;| _|   >  <   \ \/\/ / | |\/| |
;;|___| /_/\_\   \_/\_/  |_|  |_|
;; === FIX FOR CHROME SCREENSHOT ISSUE ===
;; REPLACE your current EXWM configuration with this integrated version


;; =========================================
;; EXWM and Linux-specific sections REMOVED
;; Lines 242-466 from original config
;; (EXWM, dmenu, rofi keybindings)
;; =========================================

;; ============================================
;; WINDOW RESIZING - Option + Arrow Keys
;; ============================================
;; Smart window resize function (context-aware)
(defun my/smart-resize-window (direction)
  "Intelligently resize window based on position and direction.
Automatically inverts behavior for bottom/rightmost windows."
  (let* ((window (selected-window))
         (is-rightmost (= (nth 2 (window-edges window)) (frame-width)))
         (is-bottommost (= (nth 3 (window-edges window)) (frame-height))))
    (pcase direction
      ('right (enlarge-window-horizontally 10))
      ('left (shrink-window-horizontally 10))
      ('down (if is-bottommost
                 (shrink-window 10)
               (enlarge-window 10)))
      ('up (if is-bottommost
               (enlarge-window 10)
             (shrink-window 10))))))

;; Keybindings: Option + Arrow Keys (Mac-friendly)
(global-set-key (kbd "M-<up>")
                (lambda ()
                  (interactive)
                  (my/smart-resize-window 'up)))

(global-set-key (kbd "M-<down>")
                (lambda ()
                  (interactive)
                  (my/smart-resize-window 'down)))

(global-set-key (kbd "M-<right>")
                (lambda ()
                  (interactive)
                  (my/smart-resize-window 'right)))

(global-set-key (kbd "M-<left>")
                (lambda ()
                  (interactive)
                  (my/smart-resize-window 'left)))

;; Auto Save
;;
;;____              ___ ________   ______________ ______      ___   ____         __________   ____       ____   ____       ____
;;`Mb(      db      )d' `MMMMMMMb. `MM'MMMMMMMMMM `MM`MM\     `M'  6MMMMb/       MMMMMMMMMM  6MMMMb     6MMMMb  `MM'      6MMMMb\
;; YM.     ,PM.     ,P   MM    `Mb  MM /   MM   \  MM MMM\     M  8P    YM       /   MM   \ 8P    Y8   8P    Y8  MM      6M'    `
;; `Mb     d'Mb     d'   MM     MM  MM     MM      MM M\MM\    M 6M      Y           MM    6M      Mb 6M      Mb MM      MM
;;  YM.   ,P YM.   ,P    MM     MM  MM     MM      MM M \MM\   M MM                  MM    MM      MM MM      MM MM      YM.
;;  `Mb   d' `Mb   d'    MM    .M9  MM     MM      MM M  \MM\  M MM                  MM    MM      MM MM      MM MM       YMMMMb
;;   YM. ,P   YM. ,P     MMMMMMM9'  MM     MM      MM M   \MM\ M MM     ___          MM    MM      MM MM      MM MM           `Mb
;;   `Mb d'   `Mb d'     MM  \M\    MM     MM      MM M    \MM\M MM     `M'          MM    MM      MM MM      MM MM            MM
;;    YM,P     YM,P      MM   \M\   MM     MM      MM M     \MMM YM      M           MM    YM      M9 YM      M9 MM            MM
;;    `MM'     `MM'      MM    \M\  MM     MM      MM M      \MM  8b    d9           MM     8b    d8   8b    d8  MM    / L    ,M9
;;     YP       YP      _MM_    \M\_MM_   _MM_    _MM_M_      \M   YMMMM9           _MM_     YMMMM9     YMMMM9  _MMMMMMM MYMMMM9

(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")
;;(setq doom-theme 'eink-newspaper)

;; From orginal Bearbait LLC emacs
(setq mw-thesaurus-api-key (get-env-or-warn "MW_THESAURUS_API_KEY"))
;; Distraction-free screen
;;(add-to-list 'load-path "~/.emacs.d/modules/editor/olivetti/")
;; Ensure use-package is installed and required
;;(unless (package-installed-p 'use-package)
 ;; (package-refresh-contents)
 ;; (package-install 'use-package))
;;(require 'use-package)
(require 'olivetti)
(use-package olivetti
  :init
  (setq olivetti-body-width 0.67)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  ;; General scrolling settings to help with cursor jumping
  (setq scroll-conservatively 101)
  (setq scroll-margin 5)
  (setq scroll-preserve-screen-position t)
  (setq auto-window-vscroll t)
  (setq auto-hscroll-mode nil)

  :config
  ;; Enable visual-line-mode when olivetti-mode is activated
  (add-hook 'olivetti-mode-hook 'visual-line-mode)

  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if olivetti-mode
        (progn
          (jump-to-register 1)
          (olivetti-mode -1)
          (display-line-numbers-mode 1)
          (text-scale-decrease 2)
          (setq-local line-spacing nil)
          (writegood-mode -1)
          (auto-save-visited-mode -1)
          (wc-mode -1)
          (flyspell-mode -1)
          ;;(wordnut-lookup-current-word -1)
          ;;(oxford-dictionary-lookup-current-word -1)
          ;;(magit-mode -1)
          ;; (undo-tree-mode -1)
          (synosaurus-mode -1)
          (emacs-lock-mode -1)
          (visual-line-mode -1))  ; Disable visual-line-mode when exiting
      (progn
        (window-configuration-to-register 1)
        (delete-other-windows)
        (text-scale-increase 2)
        (display-line-numbers-mode 0)
        (olivetti-mode 1)
        (setq-local line-spacing 0.75)
        (writegood-mode 1)
        (auto-save-visited-mode 1)
        (wc-mode 1)
        (turn-on-flyspell)
        ;;(wordnut-lookup-current-word 1)
        ;;(oxford-dictionary-lookup-current-word 1)
        ;;(magit-mode 1)
        ;;(undo-tree-mode 1)
        (synosaurus-mode 1)
        (emacs-lock-mode 'kill)
        (visual-line-mode 1))))  ; Enable visual-line-mode when entering
  :bind
  (("<f9>" . distraction-free)))

;; If you're using Org mode, these settings might help
(with-eval-after-load 'org
  (setq org-startup-folded nil)
  (setq org-fold-catch-invisible-edits 'show-and-error))

;; LaTex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
;;
;;
;; Ensure ox-epub is loaded
;;(use-package 'ox-epub)

;; Set PDF style and markup settings for Org mode
(after! org
  (require 'ox-latex)
  (setq org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-latex-compiler "lualatex"
        TeX-engine 'luatex
        LaTeX-command "lualatex"))

(setq org-latex-classes
        '(("article"
           "\\documentclass[11pt]{article}
            \\usepackage{fontspec}
            \\usepackage{unicode-math}
            \\usepackage{amsmath}
            \\usepackage{amssymb}
            \\usepackage{graphicx}
            \\usepackage{hyperref}
            \\usepackage[margin=1in]{geometry}
            \\usepackage{microtype}
            \\usepackage{csquotes}
            \\usepackage{lmodern}
            % Disable automatic table of contents
            \\renewcommand\\tableofcontents{}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (setq org-latex-default-class "article")

  ;; Disable table of contents in Org export
  (setq org-latex-toc-command nil)
    ;; Ensure LaTeX export backend is available
;;   (add-to-list 'org-export-backends 'latex)

;; B L O G
;; ORG ROAM FROM CLAUD
;; TIP: Don't use your existing Org files folder for this!
;; Your existing files won't have the necessary metadata
;; so they won't show up in Org Roam's database. from
;; [[org:germanlit/roaminstructions.org][From claude.]]
(use-package! org-roam
  :init
  (setq org-roam-v2-ack t) ; Acknowledge Org Roam v2
  :custom
  (org-roam-directory "~/cloud/org/roam") ; Set your desired directory for notes
  (org-roam-db-location "~/.doom.d/.local/org-roam-db/org-roam.db") ; Set local directory for the database
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  ;; Ensure the local database directory exists
  (let ((db-dir (file-name-directory org-roam-db-location)))
    (unless (file-exists-p db-dir)
      (make-directory db-dir t)))
  (org-roam-db-autosync-mode))

;; Node display configuration
;;(after! org-roam
;;  (setq org-roam-node-display-template
;;        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

;; Capture templates
;;
;; Graph visualization settings
;; Graph visualization settings with dot to SVG conversion
;; Graph visualization settings with sxiv
;;(after! org-roam
;; (setq org-roam-graph-viewer
;;     (lambda (file)
;;        (let ((svg-file "/tmp/org-roam-graph.svg"))
;;          ;; Convert to SVG
;;          (call-process "dot" nil nil nil "-Tsvg" file "-o" svg-file)
;;          (message "SVG saved to: %s" svg-file)
;;          ;; Open with sxiv
;;          (async-shell-command
;;           (format "sxiv %s" svg-file)
;;           "*org-roam-graph*"))))
;;  (setq org-roam-graph-extra-config '(("rankdir" . "LR")
;;                                     ("bgcolor" . "#00000000")))
;;  (setq org-roam-graph-node-extra-config '(("color" . "#11111111")))
;;  (setq org-roam-graph-edge-extra-config '(("color" . "#33333333"))))

;;(defun my/org-roam-graph-by-tag (tag-name)
;; (let ((nodes (seq-filter
;;               (my/org-roam-filter-by-tag tag-name)
;;                (org-roam-node-list))))
;;    (org-roam-graph--build
;;     :node-query (format "SELECTED-NODES
;;                         WHERE id in {%s}"
;;                        (string-join
;;                         (mapcar (lambda (n)
;;                           (format "'%s'" (org-roam-node-id n)))
;;                         nodes)
;;                         ",")))))
;; Daily notes configuration
(use-package! org-roam-dailies
  :after org-roam
  :config
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))

;; Org-roam-ui configuration
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-browser-function 'eww-browse-url) ; Use eww for internal browser
  (setq org-roam-ui-open-on-start t)) ; Open UI on Emacs start

(after! ox-hugo
  (setq org-hugo-base-dir "~/cloud.bear/writing/blog")
  (setq org-hugo-section "posts"))
;;
;;LEXIC Dictionary https://tecosaur.github.io/emacs-config/config.html#dictionary
;; Set dictionary path explicitly for lexic/sdcv
(setenv "STARDICT_DATA_DIR" (expand-file-name "~/.local/share/stardict"))

(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  ;; Increase timeout to avoid "waiting for lexic" errors
  (setq lexic-idle-timeout 10)

  ;; Explicitly set the dictionary path
  (setq lexic-dictionary-path "~/.local/share/stardict/dic")

  ;; Key bindings
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))

;; Override Doom's dictionary lookup to use lexic
(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))

(map! :leader
      (:prefix ("d" . "dictionary")
       :desc "Search word in dictionary" "s" #'lexic-search))

(after! lexic
  (set-popup-rule! "^\\*Lexic\\*"
    :side 'bottom
    :size 0.3
    :select t
    :quit 'current))
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                                                                                                 ;;
;;;
;;PPPPPPPPPPPP
;;PP        PP
;;PP        PP
;;PPPPPPPPPPPP
;;PP
;;PP
;;PP
;;PP
;;PP                   r i n t i n g ha h

; Printing configuration
(after! org
  ;; Printer settings
  (setq printer-name "brother_hl-l2320d_series")
  (setq lpr-command "lp")
  (setq lpr-add-switches nil)  ; Don't use lpr switches with lp

  ;; Basic lp switches
  (setq lp-switches `("-d" ,printer-name))

  ;; Function to print the current buffer or region
  (defun my/print-buffer-or-region ()
    (interactive)
    (let* ((start (if (use-region-p) (region-beginning) (point-min)))
           (end (if (use-region-p) (region-end) (point-max)))
           (print-command (concat lpr-command " "
                                  (mapconcat 'identity lp-switches " ")))
           (temp-file (make-temp-file "emacs-print-")))
      (write-region start end temp-file)
      (message "Executing print command: %s %s" print-command temp-file)
      (shell-command-to-string (concat print-command " " temp-file))
      (delete-file temp-file)))

  ;; Function to print a file (e.g., exported PDF)
  (defun my/print-file (file)
    (interactive "fFile to print: ")
    (let ((print-command (concat lpr-command " "
                                 (mapconcat 'identity lp-switches " ")
                                 " " (shell-quote-argument file))))
      (message "Executing print command: %s" print-command)
      (shell-command-to-string print-command)))

  ;; Advice to run after PDF export
  (advice-add 'org-latex-export-to-pdf :after
              (lambda (file-name subtitle &optional body-only ext-plist post-process)
                (when (yes-or-no-p "Do you want to print the exported PDF?")
                  (my/print-file (concat (file-name-sans-extension file-name) ".pdf"))))))

;; Override the default print functions
(defalias 'print-buffer 'my/print-buffer-or-region)
(defalias 'print-region 'my/print-buffer-or-region)

;; Global key binding for quick printing
(global-set-key (kbd "C-c p") 'my/print-buffer-or-region);; Optional: Set default fonts if you want to use specific fonts
  ;; (add-to-list 'org-latex-packages-alist
  ;;              '("" "\/\setmainfont{DejaVu Serif}

  ;;                  \\setsansfont{DejaVu Sans};;

  ;; Ensure LaTeX export backend is available
(after! org
  (add-to-list 'org-export-backends 'latex));;some configs inspired by  https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/
;;genreally just trying to make orgmode look better for writing and edditng.
;;
(setq doom-modeline-enable-word-count t)
;;  auto-save-default makes copies of your files in ~/.emacs.d/.local/cache/{autosave,backup}, then deletes them when you save the buffer. After a crash, they can be restored with M-x and recover-file, recover-session, or recover-this-file.#
;; Global auto-save settings
(setq auto-save-default t
      make-backup-files t
      auto-save-interval 20
      auto-save-timeout 10)

;; Enable auto-save-visited-mode globally
(auto-save-visited-mode +1)

;; Enable auto-save-mode for specific major modes if needed
;;(add-hook 'org-mode-hook #'auto-save-mode)

;; If you still want the additional buffer saving timer
;;(run-with-idle-timer 30 t #'save-some-buffers)
(defun toggle-line-spacing ()
  "toggle line spacing between no extra space to extra half line height.
url `http://xahlee.info/emacs/emacs/emacs_toggle_line_spacing.html'
version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing .75))
  (redraw-frame (selected-frame)))
;; Configure Langtool
(use-package! langtool
  :config
  (setq langtool-language-tool-jar "/path/to/languagetool-commandline.jar")
  (setq langtool-default-language "en-US")
  :bind (("C-c l c" . langtool-check)
         ("C-c l d" . langtool-check-done)
         ("C-c l s" . langtool-switch-default-language)
         ("C-c l m" . langtool-show-message-at-point)))
;; Configure writegood-mode
(use-package! writegood-mode
  :hook (text-mode markdown-mode org-mode))
;; Configure wordnut
(use-package! wordnut
  :bind ("C-c w" . wordnut-lookup-current-word))
;; Configure synosaurus
(use-package! synosaurus
  :config
  (setq synosaurus-backend 'synosaurus-backend-wordnet)
  :bind ("C-c s" . synosaurus-choose-and-replace))
(setq python-shell-interpreter "python3") ;; Replace "pythonx" with the actual path if needed
;;
;;PPPPPPPPPPPP
;;PP        PP
;;PP        PP
;;PPPPPPPPPPPP
;;PP
;;PP
;;PP
;;PP
;;PP                   r i n t i n g ha h

;; Use comma to insert variable                                        ;
;;      Use comma to insert variable; Optional: Disable printing the header
;;(setq org-latex-compiler "lualatex")

;;
;;export to pdf from org
(after! org
  (require 'ox-latex)
  (setq org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-latex-classes
        '(("article"
           "\\documentclass[11pt]{article}
            \\usepackage{fontspec}
            \\usepackage{unicode-math}
            \\usepackage{amsmath}
            \\usepackage{amssymb}
            \\usepackage{graphicx}
            \\usepackage{hyperref}
            \\usepackage[margin=1in]{geometry}
            \\usepackage{microtype}
            \\usepackage{csquotes}
            \\setmainfont{DejaVu Serif}
            \\setsansfont{DejaVu Sans}
            \\setmonofont{DejaVu Sans Mono}
            % Disable automatic table of contents
            \\renewcommand\\tableofcontents{}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("beamer"
           "\\documentclass[presentation]{beamer}
            \\usepackage{fontspec}
            \\usepackage{unicode-math}"
           ("\\section{%s}" . "\\section*{%s}")))))

(after! tex
  (setq-default TeX-engine 'luatex
                TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-command-extra-options "-shell-escape")))
  
  (setq org-latex-default-class "article"
        org-latex-compiler "lualatex"
        TeX-engine 'luatex
        LaTeX-command "lualatex"
        org-latex-toc-command nil)
  
  (add-to-list 'org-export-backends 'latex))

;; Configure Emacs to use Okular for opening PDF files
;;
;;(setq org-file-apps
;;      '((auto-mode . emacs)
;;        ("\\.pdf\\'" . "okular %s")));;
;;
;;
;;░▒▓████████▓▒░▒▓██████▓▒░ ░▒▓███████▓▒░▒▓████████▓▒░      ░▒▓████████▓▒░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓█▓▒░       ░▒▓███████▓▒░
;;░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░         ░▒▓█▓▒░             ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░
;;░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░         ░▒▓█▓▒░             ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░
;;░▒▓██████▓▒░░▒▓████████▓▒░░▒▓██████▓▒░   ░▒▓█▓▒░             ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░       ░▒▓██████▓▒░
;;░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░  ░▒▓█▓▒░             ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░
;;░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░  ░▒▓█▓▒░             ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░
;;░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░   ░▒▓█▓▒░             ░▒▓█▓▒░   ░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓████████▓▒░▒▓███████▓▒░
;;
;;Treemacs configs. Trying to get out of the .doom dir
(after! treemacs
  (setq treemacs-width 30
        treemacs-follow-mode t
        treemacs-git-mode 'deferred
        treemacs-show-hidden-files t))

;; Enable tramp mode for remote file editing
(setq tramp-default-method "ssh")
;; Map or ReMap Keybindings according chatgpt and https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
;;(map! "C-c t" #'gts-do-translate)
(map! "C-c j" #'evil-join)

;; ORG MODE CHEATSHEET space c c
(defun my/open-org-cheatsheet ()
  "Open the Org Mode cheatsheet file."
  (interactive)
  (find-file "~/cloud/org/orgstyle-cheatsheet.org"))
(map! :leader
      :desc "Open Org Cheatsheet" "c c" #'my/open-org-cheatsheet)
;

;; Define function to open keybindings cheatsheet
(defun my/open-keybindings-cheatsheet ()
  "Open the keybindings cheatsheet file."
  (interactive)
  (find-file "~/cloud/org/cheatsheets/keybindings.org"))

;; Create the keybinding using map! macro
(map! :leader
      (:prefix ("k" . "keybindings")
       :desc "Open keybindings cheatsheet" "b" #'my/open-keybindings-cheatsheet))

;; Key binding to open Terminal (macOS)
(defun my/open-terminal ()
  "Open Terminal.app on macOS."
  (interactive)
  (shell-command "open -a Terminal"))

(map! :leader
      :desc "Open Terminal" "t a" #'my/open-terminal)

;; Open Chrome on macOS - space w c
(defun my/open-chrome ()
  "Open Google Chrome on macOS."
  (interactive)
  (shell-command "open -a 'Google Chrome'"))

;; Keybinding for Chrome
(map! :leader
      :desc "Open Chrome" "w c" #'my/open-chrome)

;; eww search engine switcher
(defvar my/eww-search-engines
  '(("kagi" . "https://kagi.com/search?q=")
    ("dogpile" . "https://www.dogpile.com/search?q=")
    ("duckduckgo" . "https://duckduckgo.com/?q=")
    ("startpage" . "https://www.startpage.com/search?q=")
    ("mojeek" . "https://www.mojeek.com/search?q="))
  "List of search engines for eww")

(defun my/eww-set-search (engine)
  "Switch eww search engine"
  (interactive
   (list (completing-read
          "Search engine: "
          (mapcar #'car my/eww-search-engines))))
  (setq eww-search-prefix (cdr (assoc engine my/eww-search-engines)))
  (message "Search engine set to: %s" engine))

(after! eww
  ;; Set default search engine to Kagi
  (setq eww-search-prefix "https://kagi.com/search?q=")

  ;; Bind search engine switcher in eww buffers
  (map! :map eww-mode-map
        :n "gs" #'my/eww-set-search))

;; Enable undo-tree in every buffer by default
(global-undo-tree-mode 1)

;; Optional: Customize undo-tree settings
(after! undo-tree
;; Prevent undo-tree files from being saved
;;  (setq undo-tree-auto-save-history nil)

  ;; Set the directory for undo-tree history files
  (setq undo-tree-history-directory-alist '(("." . "~/cloud/org/undo-tree-history/")))

  ;; Optional: Increase the history length
  (setq undo-tree-history-size 10000)
  )

(setq md4rd-subs-active '(lisp+Common_Lisp emacs prolog))
(use-package! claude-shell
  :config
  (setq! claude-shell-api-token (get-env-or-warn "ANTHROPIC_API_KEY")))
;; Enable flyspell in text modes
(add-hook! '(text-mode-hook markdown-mode-hook org-mode-hook)
           #'flyspell-mode)

;; Mediawiki
(after! mediawiki
  (setq mediawiki-site-alist
        `(("wiki.bear" "https://wiki.bear/" "forrest" ,(get-env-or-warn "MEDIAWIKI_PASSWORD")))))
(after! ox-mediawiki
  (setq org-mediawiki-server-url "https://wiki.bear/"
        org-mediawiki-user "forrest"
        org-mediawiki-publish-url "https://wiki.bear/"
        org-mediawiki-default-category "wiki"))

;; Custom function remains unchanged
(defun my-whats-that-squiggle ()
  (interactive)
  (let* ((interesting-properties '(flycheck-error flyspell-overlay flymake-overlay))
         (present-properties (cl-remove-if-not
                              (lambda (p) (get-char-property (point) p))
                              interesting-properties)))
    (if present-properties
        (message "This squiggle represents: %s"
                 (mapconcat 'symbol-name present-properties ", "))
      (message "No interesting properties present"))))


;;agenda
(use-package! org
  :config
  (setq org-directory "~/cloud/org/"
        org-agenda-files '("~/cloud/org/diary2024.org"
                          "~/cloud/org/diary2025.org"
                          "~/cloud/org/todo.org"
                          "~/cloud/org/habits.org"))

  ;; Configure habit module
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  ;; Configure TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))))

;; Configure Org Agenda settings
(after! org-agenda
  (setq org-agenda-start-on-weekday 0
        org-agenda-span 7
        org-agenda-include-deadlines t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-timegrid-use-ampm t
        org-agenda-start-time "8:00"
        org-agenda-end-time "20:00")

  ;; Set agenda keybinding
  (map! :leader
        :desc "Org Agenda"
        "a" #'org-agenda))

;; Set Python interpreter to Python 3
(after! python
  (setq python-shell-interpreter "python3"))


(defun clean-pdf-text (begin end)
  "Clean up text copied from PDF, removing awkward line breaks.
Apply this to a region (BEGIN END) of text from a PDF.
Preserves paragraph breaks but removes mid-sentence line breaks."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      ;; Replace multiple spaces with single space
      (goto-char (point-min))
      (while (search-forward-regexp "  +" nil t)
        (replace-match " "))

      ;; Join lines that don't end in period, question mark, or exclamation
      (goto-char (point-min))
      (while (re-search-forward "\\([^.!?]\\)\\(\n\\)\\([^\\$]\\)" nil t)
        (replace-match "\\1 \\3"))

      ;; Remove any resulting extra spaces
      (goto-char (point-min))
      (while (search-forward-regexp " +" nil t)
        (replace-match " "))

      ;; Preserve paragraph breaks (double newlines)
      (goto-char (point-min))
      (while (search-forward-regexp "\n\n+" nil t)
        (replace-match "\n\n"))

      ;; Clean up any spaces before newlines
      (goto-char (point-min))
      (while (search-forward-regexp " +\n" nil t)
        (replace-match "\n"))

      ;; Ensure text ends with single newline
      (goto-char (point-max))
      (unless (looking-back "\n")
        (insert "\n")))))



;; Mobile Org Integration - Add to your config.el

;; Update org directory structure
(after! org
  ;; Update agenda files to include new structure
  (setq org-agenda-files '("~/cloud/org/main/diary2025.org"
                          "~/cloud/org/todo.org"
                          "~/cloud/org/habits.org"
                          "~/cloud/org/mobile/inbox.org"
                          "~/cloud/org/main/projects.org"))

  ;; Mobile org settings
  (setq org-mobile-directory "~/cloud/org/mobile/"
        org-mobile-inbox-for-pull "~/cloud/org/mobile/inbox.org")

  ;; Capture templates: Daily Reminders only (r)
  ;; Toolkit Unit Test handled via org-roam-capture-templates below
  (setq org-capture-templates
        '(("r" "Daily Reminders" entry
           (file+datetree "~/cloud/org/main/diary2025.org")
           "* Daily Tasks - %<%A, %B %d, %Y>\n- [ ] Write 500 words\n- [ ] Read for 1 hour (morning or before bed)\n- [ ] Practice German")))

  ;; Org-roam capture templates for toolkit testing
  (after! org-roam
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
             :unnarrowed t)
            ("u" "Toolkit Unit Test" plain
             (file "~/.doom.d/templates/toolkit-unit-test.org")
             :target (file+head "toolkit/unit-${slug}.org"
                                "#+title: ${title}\n#+filetags: :toolkit:testing:\n")
             :unnarrowed t)))))

;; Fixed export function - replace the current one in your config
(defun export-diary-todos-to-mobile ()
  "Export TODO items from diary to mobile-readable file."
  (interactive)
  (let ((mobile-file "~/cloud/org/mobile/diary-todos.org")
        (diary-file "~/cloud/org/main/diary2025.org"))
    (when (file-exists-p diary-file)
      ;; Read diary file in org-mode context
      (with-current-buffer (find-file-noselect diary-file)
        (save-excursion
          (goto-char (point-min))

          ;; Prepare the mobile file content
          (let ((mobile-content (concat
                                "#+TITLE: Diary TODOs (Mobile View - READ ONLY)\n"
                                "#+DESCRIPTION: Auto-generated from diary2025.org\n"
                                "#+READONLY: t\n"
                                "#+FILETAGS: :mobile:readonly:diary:\n"
                                (format "#+UPDATED: %s\n\n" (format-time-string "%Y-%m-%d %H:%M"))
                                "* ⚠️ READ ONLY - Auto-generated from main diary ⚠️\n\n"))
                (todos '()))

            ;; Extract TODO items using simple regex (avoiding org-element in temp buffer)
            (while (re-search-forward "^\\*+ \\(TODO\\|DOING\\|WAITING\\|NEXT\\)" nil t)
              (let* ((start (line-beginning-position))
                     ;; Find end of this heading (next heading or end of buffer)
                     (end (save-excursion
                            (forward-line 1)
                            (if (re-search-forward "^\\*+ " nil t)
                                (line-beginning-position)
                              (point-max))))
                     (todo-text (buffer-substring start end)))
                (push todo-text todos)))

            ;; Write to mobile file
            (with-temp-file mobile-file
              (insert mobile-content)
              (if todos
                  (progn
                    (insert "* Current TODOs\n\n")
                    (dolist (todo (reverse todos))
                      (insert todo "\n")))
                (insert "* No active TODOs found\n\n"))
              (insert "\n* Instructions\n")
              (insert "- View tasks here on mobile\n")
              (insert "- Mark complete in Emacs\n")
              (insert "- File auto-updates when diary is saved\n"))))

        (message "Diary TODOs exported to mobile view")))))

;; Auto-export on diary save
(defun auto-export-diary-todos ()
  "Auto-export diary todos when diary file is saved."
  (when (and (buffer-file-name)
             (string-match "diary2025\\.org$" (buffer-file-name)))
    (export-diary-todos-to-mobile)))

(add-hook 'after-save-hook #'auto-export-diary-todos)

;; Process mobile inbox function
(defun process-mobile-inbox ()
  "Process items from mobile inbox into main org files."
  (interactive)
  (find-file "~/cloud/org/mobile/inbox.org")
  (goto-char (point-min))
  (when (search-forward "* Quick Captures" nil t)
    (forward-line 1)
    (message "Mobile inbox opened. Use C-c C-w to refile items, or process manually.")))

;; Check for mobile conflicts
(defun check-mobile-conflicts ()
  "Check for sync conflicts in mobile org files."
  (interactive)
  (let ((conflicts (directory-files-recursively
                   "~/cloud/org/mobile/"
                   ".*conflicted.*\\.org$")))
    (if conflicts
        (progn
          (message "Found %d mobile conflict files:" (length conflicts))
          (dolist (file conflicts)
            (message "  %s" file))
          (when (y-or-n-p "Open conflicts for resolution? ")
            (dolist (file conflicts)
              (find-file file))))
      (message "No mobile conflict files found"))))

;; Enhanced mobile keybindings
(map! :leader
      (:prefix ("m" . "mobile")
       :desc "Export diary TODOs" "d" #'export-diary-todos-to-mobile
       :desc "Process mobile inbox" "p" #'process-mobile-inbox
       :desc "Check mobile conflicts" "c" #'check-mobile-conflicts
       :desc "Open mobile inbox" "i" #'(lambda ()
                                        (interactive)
                                        (find-file "~/cloud/org/mobile/inbox.org"))
       :desc "Open diary TODOs view" "v" #'(lambda ()
                                           (interactive)
                                           (find-file "~/cloud/org/mobile/diary-todos.org"))))

;; Auto-revert for mobile files
(add-hook 'find-file-hook
          (lambda ()
            (when (string-match "mobile/" (or (buffer-file-name) ""))
              (auto-revert-mode 1)
              (setq-local auto-revert-verbose nil))))

;; Backup mobile files before sync
(defun backup-mobile-file ()
  "Create backup of mobile file before external modification."
  (when (and (buffer-file-name)
             (string-match "mobile/.*\\.org$" (buffer-file-name)))
    (let* ((backup-dir "~/cloud/org/mobile/backups/")
           (file-name (file-name-nondirectory (buffer-file-name)))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (backup-file (concat backup-dir file-name "-" timestamp ".backup")))
      (make-directory backup-dir t)
      (copy-file (buffer-file-name) backup-file)
      (message "Mobile file backed up to %s" backup-file))))

(add-hook 'before-save-hook #'backup-mobile-file)



;;EEEEEEEEEEEEEEEEEEEEEEMMMMMMMM               MMMMMMMM               AAA               IIIIIIIIIILLLLLLLLLLL
;;E::::::::::::::::::::EM:::::::M             M:::::::M              A:::A              I::::::::IL:::::::::L                  
;;E::::::::::::::::::::EM::::::::M           M::::::::M             A:::::A             I::::::::IL:::::::::L                  
;;EE::::::EEEEEEEEE::::EM:::::::::M         M:::::::::M            A:::::::A            II::::::IILL:::::::LL                  
;;  E:::::E       EEEEEEM::::::::::M       M::::::::::M           A:::::::::A             I::::I    L:::::L                    
;;  E:::::E             M:::::::::::M     M:::::::::::M          A:::::A:::::A            I::::I    L:::::L                    
;;  E::::::EEEEEEEEEE   M:::::::M::::M   M::::M:::::::M         A:::::A A:::::A           I::::I    L:::::L                    
;;  E:::::::::::::::E   M::::::M M::::M M::::M M::::::M        A:::::A   A:::::A          I::::I    L:::::L                    
;;  E:::::::::::::::E   M::::::M  M::::M::::M  M::::::M       A:::::A     A:::::A         I::::I    L:::::L                    
;;  E::::::EEEEEEEEEE   M::::::M   M:::::::M   M::::::M      A:::::AAAAAAAAA:::::A        I::::I    L:::::L                    
;;  E:::::E             M::::::M    M:::::M    M::::::M     A:::::::::::::::::::::A       I::::I    L:::::L                    
;;  E:::::E       EEEEEEM::::::M     MMMMM     M::::::M    A:::::AAAAAAAAAAAAA:::::A      I::::I    L:::::L         LLLLLL     
;;EE::::::EEEEEEEE:::::EM::::::M               M::::::M   A:::::A             A:::::A   II::::::IILL:::::::LLLLLLLLL:::::L     
;;E::::::::::::::::::::EM::::::M               M::::::M  A:::::A               A:::::A  I::::::::IL::::::::::::::::::::::L     
;;E::::::::::::::::::::EM::::::M               M::::::M A:::::A                 A:::::A I::::::::IL::::::::::::::::::::::L     
;;EEEEEEEEEEEEEEEEEEEEEEMMMMMMMM               MMMMMMMMAAAAAAA                   AAAAAAAIIIIIIIIIILLLLLLLLLLLLLLLLLLLLLLLL     
;;                                                                                                                             
;; Configure your Gmail account

;; Enable mu4e and related configurations
;; Configure your Gmail account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (after! mu4e                                                                                                  ;;
;;   ;; Basic settings                                                                                           ;;
;;   (setq mu4e-root-maildir "~/.mail"                                                                           ;;
;;         mu4e-get-mail-command "mbsync -a"                                                                     ;;
;;         mu4e-update-interval 300 ;; 300 seconds = 5 minutes                                                   ;;
;;         mu4e-compose-format-flowed t                                                                          ;;
;;         mu4e-compose-dont-reply-to-self t                                                                     ;;
;;         mu4e-compose-signature-auto-include t                                                                 ;;
;;         mu4e-compose-signature (concat "Regards,\n" user-full-name)                                           ;;
;;         mu4e-index-cleanup nil                                                                                ;;
;;         mu4e-index-lazy-check t)                                                                              ;;
;;                                                                                                               ;;
;;   ;; Prevent multiple update processes                                                                        ;;
;;   (setq mu4e-update-process-automatically-p t                                                                 ;;
;;         mu4e-update-allow-multiple-updates nil                                                                ;;
;;         mu4e-update-pre-hook (lambda ()                                                                       ;;
;;                               (when (process-live-p (get-process "mu4e-update"))                              ;;
;;                                 (kill-process "mu4e-update"))))                                               ;;
;;   ;; Set up Gmail account                                                                                     ;;
;;   (set-email-account! "Gmail"                                                                                 ;;
;;     '((mu4e-sent-folder       . "/Sent Mail")                                                                 ;;
;;       (mu4e-drafts-folder     . "/Drafts")                                                                    ;;
;;       (mu4e-trash-folder      . "/Trash")                                                                    ;;
;;       (mu4e-refile-folder     . "/All Mail")                                                                  ;;
;;       (smtpmail-smtp-user     . "forrestmuelrath@gmail.com")                                                  ;;
;;       (user-mail-address      . "forrestmuelrath@gmail.com")                                                  ;;
;;       (mu4e-compose-signature . "---\nForrest Muelrath"))                                                     ;;
;;     t)                                                                                                        ;;
;;                                                                                                               ;;
;;   ;; Gmail-specific settings                                                                                  ;;
;;   (setq +mu4e-gmail-accounts '(("forrestmuelrath@gmail.com" . "/"))                                           ;;
;;         mu4e-context-policy 'ask-if-none                                                                      ;;
;;         mu4e-compose-context-policy 'always-ask)                                                              ;;
;;                                                                                                               ;;
;;    ;; Email sending configuration using SMTP directly -- comment out if you want to use.                      ;;
;;    (setq send-mail-function 'smtpmail-send-it                                                                 ;;
;;          message-send-mail-function 'smtpmail-send-it                                                         ;;
;;          smtpmail-stream-type 'starttls                                                                       ;;
;;          smtpmail-smtp-server "smtp.gmail.com"                                                                ;;
;;          smtpmail-smtp-service 587                                                                            ;;
;;          smtpmail-smtp-user "forrestmuelrath@gmail.com"                                                       ;;
;;          smtpmail-debug-info t)                                                                               ;;
;;                                                                                                               ;;
;;   ;; Email sending configuration IF YOU WANT TO TRY MSMTP FOR ORGSMG FORMATING TRICKS                         ;;
;; ;; (setq send-mail-function 'message-send-mail-with-sendmail                                                  ;;
;; ;;        message-send-mail-function 'message-send-mail-with-sendmail                                         ;;
;; ;;       sendmail-program "/usr/bin/msmtp"                                                                    ;;
;; ;;        message-sendmail-extra-arguments '("--read-envelope-from")                                          ;;
;; ;;        message-sendmail-f-is-evil t)                                                                       ;;
;;                                                                                                               ;;
;;   ;; SMTP configuration as fallback                                                                           ;;
;; ;;  (setq smtpmail-stream-type 'starttls                                                                      ;;
;; ;;        smtpmail-default-smtp-server "smtp.gmail.com"                                                       ;;
;; ;;        smtpmail-smtp-server "smtp.gmail.com"                                                               ;;
;; ;;        smtpmail-smtp-service 587)                                                                          ;;
;;                                                                                                               ;;
;;   ;; If you're using msmtp, uncomment and modify these lines                                                  ;;
;;   ;; (setq sendmail-program (executable-find "msmtp")                                                         ;;
;;   ;;       send-mail-function 'smtpmail-send-it  ; Use 'message-send-mail-with-sendmail if msmtp is preferred ;;
;;   ;;       message-sendmail-f-is-evil t                                                                       ;;
;;   ;;       message-sendmail-extra-arguments '("--read-envelope-from")                                         ;;
;;   ;;       message-send-mail-function 'message-send-mail-with-sendmail)                                       ;;
;; )                                                                                                             ;;
;;                                                                                                               ;;
;; ;; Optional: Improve readability                                                                              ;;
;; (setq shr-color-visible-luminance-min 80                                                                      ;;
;;       shr-use-colors nil)                                                                                     ;;
;;                                                                                                               ;;
;; ;; Optional: Use variable-pitch fonts in mu4e view mode                                                       ;;
;; (add-hook 'mu4e-view-mode-hook #'variable-pitch-mode)                                                         ;;
;;                                                                                                               ;;
;; ;; Enable background mail fetching and notifications                                                          ;;
;; (use-package! mu4e-alert                                                                                      ;;
;;   :after mu4e                                                                                                 ;;
;;   :config                                                                                                     ;;
;;   (mu4e-alert-set-default-style 'libnotify)                                                                   ;;
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)                                               ;;
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))                                          ;;
;;                                                                                                               ;;
;; (setq mu4e-index-update-in-background t                                                                       ;;
;;       mu4e-hide-index-messages t)                                                                             ;;
;;                                                                                                               ;;
;;   ;; Set up HTML email composition                                                                            ;;
;; (setq mu4e-compose-format-flowed t                                                                            ;;
;;       mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum                                           ;;
;;       mu4e-html2text-command 'mu4e-shr2text                                                                   ;;
;;       shr-use-fonts t)                                                                                        ;;
;;                                                                                                               ;;
;;   ;; Set default HTML composition style                                                                       ;;
;; (setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"                                             ;;
;;       message-citation-line-function 'message-insert-formatted-citation-line                                  ;;
;;       message-kill-buffer-on-exit t)                                                                          ;;
;;                                                                                                               ;;
;;   ;; Add HTML email headers                                                                                   ;;
;; (add-to-list 'mu4e-compose-mode-hook                                                                          ;;
;;             (lambda ()                                                                                        ;;
;;               (save-excursion                                                                                 ;;
;;                 (message-goto-body)                                                                           ;;
;;                 (insert "Content-Type: text/html\n\n")                                                        ;;
;;                 (insert "<html><head><style>body { font-family: Georgia, serif; }</style></head><body>")      ;;
;;                 (goto-char (point-max))                                                                       ;;
;;                 (insert "</body></html>"))))                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;VOLUUUME KNOB???
(map! :map global-map
      "<XF86AudioMute>"        (cmd! (shell-command "amixer -q sset Master toggle"))
      "<XF86AudioLowerVolume>" (cmd! (shell-command "amixer -q sset Master 5%-"))
      "<XF86AudioRaiseVolume>" (cmd! (shell-command "amixer -q sset Master 5%+"))
      "<XF86PowerOff>"         (cmd! (shell-command "systemctl poweroff"))
      "<XF86AudioPlay>"        (cmd! (shell-command "playerctl play-pause"))
      "<XF86AudioStop>"        (cmd! (shell-command "playerctl stop"))
      "<XF86AudioPrev>"        (cmd! (shell-command "playerctl previous"))
      "<XF86AudioNext>"        (cmd! (shell-command "playerctl next")));;
;;
(after! aider
  ;; Configure for Claude 3.5 Sonnet
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))

  ;; Set your Anthropic API key
  ;; Replace "your-api-key-here" with your actual API key
  (setenv "ANTHROPIC_API_KEY" (get-env-or-warn "ANTHROPIC_API_KEY"))
  ;; Optional: Enable helm support if you have helm installed
  (when (featurep 'helm)
    (require 'aider-helm))

  ;; Optional: Enable aider-minor-mode for files with "aider" in their name
  (add-hook 'find-file-hook
            (lambda ()
              (when (and (buffer-file-name)
                         (string-match-p "aider" (buffer-file-name)))
                (aider-minor-mode 1))))

  ;; Define key bindings using Doom's map! macro
  (map! :leader
        (:prefix ("A" . "aider")
         :desc "Open aider buffer" "o" #'aider-open
         :desc "Add current file" "a" #'aider-add-current-file
         :desc "Reset session" "r" #'aider-reset
         :desc "Show transient menu" "t" #'aider-transient-menu))

  ;; Optional: Configure additional settings
  (setq aider-chat-model "anthropic/claude-3-5-sonnet-20241022")
  (setq aider-default-prompt-context "You are a helpful AI programming assistant.")
  (setq aider-code-model "anthropic/claude-3-5-sonnet-20241022"))

;; Optional: Add custom functions
(defun aider-help ()
  "Show help for aider commands."
  (interactive)
  (message "Aider Commands:
SPC A o - Open aider buffer
SPC A a - Add current file
SPC A r - Reset session
SPC A t - Show transient menu"))

;; GPTel configuration for Claude (updated)
(after! gptel
  (require 'gptel-anthropic)
  (require 'gptel-openai)

  ;; Define Anthropic backends
  (gptel-make-anthropic "Claude-Haiku-4.5"
    :stream t
    :key (get-env-or-warn "ANTHROPIC_API_KEY")
    :models '(claude-haiku-4-5-20251001))

  (gptel-make-anthropic "Claude-Sonnet-4.5"
    :stream t
    :key (get-env-or-warn "ANTHROPIC_API_KEY")
    :models '(claude-sonnet-4-5-20250929))

  ;; Optional older model (deprecated)
  (gptel-make-anthropic "Claude-Sonnet-3.7"
    :stream t
    :key (get-env-or-warn "ANTHROPIC_API_KEY")
    :models '(claude-3-7-sonnet-20250219))

  ;; Google Gemini backends
  (gptel-make-openai "Gemini-Pro"
    :host "generativelanguage.googleapis.com"
    :endpoint "/v1beta/models/gemini-pro:generateContent"
    :key (getenv "GEMINI_API_KEY")
    :models '(gemini-pro)
    :stream t)

  (gptel-make-openai "Gemini-Pro-Vision"
    :host "generativelanguage.googleapis.com"
    :endpoint "/v1beta/models/gemini-pro-vision:generateContent"
    :key (getenv "GEMINI_API_KEY")
    :models '(gemini-pro-vision)
    :stream t)

  ;; Big-Pickle (OpenAI-compatible endpoint)
  (gptel-make-openai "Big-Pickle"
    :host "api.openai.com"
    :key (get-env-or-warn "OPENAI_API_KEY")
    :models '(gpt-4-turbo-preview gpt-4 gpt-3.5-turbo)
    :stream t)

  ;; Set Haiku 4.5 as default
  (setq-default
   gptel-backend (gptel-get-backend "Claude-Haiku-4.5")
   gptel-model 'claude-haiku-4-5-20251001)

  ;; Model selection alist for switching
  (setq gptel-model-alist
        '((claude-haiku-4-5-20251001 . "Haiku 4.5")
          (claude-sonnet-4-5-20250929 . "Sonnet 4.5")
          (claude-3-7-sonnet-20250219 . "Sonnet 3.7 (deprecated)")
          (gemini-pro . "Gemini Pro")
          (gemini-pro-vision . "Gemini Pro Vision")
          (gpt-4-turbo-preview . "GPT-4 Turbo")
          (gpt-4 . "GPT-4")
          (gpt-3.5-turbo . "GPT-3.5 Turbo")))

  ;; Keybindings
  (map! :leader
        (:prefix ("l" . "LLM")
         :desc "Send buffer/region" "s" #'gptel-send
         :desc "Switch to Sonnet 4.5 (power)" "S"
           (lambda ()
             (interactive)
             (setq gptel-backend (gptel-get-backend "Claude-Sonnet-4.5")
                   gptel-model 'claude-sonnet-4-5-20250929)
             (message "Switched to Claude Sonnet 4.5"))
         :desc "Switch to Haiku 4.5 (cheap)" "h"
           (lambda ()
             (interactive)
             (setq gptel-backend (gptel-get-backend "Claude-Haiku-4.5")
                   gptel-model 'claude-haiku-4-5-20251001)
             (message "Switched to Claude Haiku 4.5"))
          :desc "Switch to Sonnet 3.7 (fallback)" "3"
            (lambda ()
              (interactive)
              (setq gptel-backend (gptel-get-backend "Claude-Sonnet-3.7")
                    gptel-model 'claude-3-7-sonnet-20250219)
              (message "Switched to Claude Sonnet 3.7 (deprecated)"))
          :desc "Switch to Gemini Pro" "g"
            (lambda ()
              (interactive)
              (setq gptel-backend (gptel-get-backend "Gemini-Pro")
                    gptel-model 'gemini-pro)
              (message "Switched to Gemini Pro"))
          :desc "Switch to Big-Pickle (GPT-4)" "b"
            (lambda ()
              (interactive)
              (setq gptel-backend (gptel-get-backend "Big-Pickle")
                    gptel-model 'gpt-4-turbo-preview)
              (message "Switched to Big-Pickle (GPT-4 Turbo)"))
         :desc "Start Claude chat" "c" #'gptel
         :desc "Rewrite region" "r" #'gptel-rewrite
         :desc "Toggle gptel mode" "t" #'gptel-mode))

  ;; Directives
  (setq gptel-directives
        '((default . "You are a helpful AI assistant. Be concise but thorough.")
          (programming . "You are a programming assistant. Provide clear, efficient code.")
          (writing . "You are a writing assistant. Focus on clarity and style.")
          (lisp . "You are a LISP expert. Provide idiomatic Emacs Lisp solutions.")))

  ;; Default tokens / streaming
  (setq gptel-max-tokens 6000
        gptel-stream t)

  ;; Your custom functions
  (defun my/claude-explain-region ()
    "Ask Claude to explain the selected region."
    (interactive)
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (gptel-request text :system "Please explain this text in detail."))
      (message "No region selected")))

  (defun my/claude-review-writing ()
    "Ask Claude to review the current buffer or region for writing improvements."
    (interactive)
    (gptel-request
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (buffer-string))
     :system "Please review this writing for grammar, style, clarity, and flow. Provide specific suggestions for improvement."))

  ;; Additional keybindings for custom functions
  (map! :leader
        (:prefix ("l" . "LLM")
         :desc "Explain region" "e" #'my/claude-explain-region
         :desc "Review writing" "w" #'my/claude-review-writing))

  ;; Popup rule (assuming you had this before)
  (set-popup-rule! "\\*ChatGPT\\*"
    :side 'right
    :size 0.4
    :select t
    :quit t))

;; Optional: Auto-enable gptel-mode in writing buffers
(add-hook 'org-mode-hook
          (lambda ()
            (when (string-match "writing\\|blog\\|article\\|diary"
                                (or (buffer-file-name) ""))
              (gptel-mode 1))))
(add-hook 'markdown-mode-hook #'gptel-mode)

;; WEATHER_SUNHINE
;; Sunshine weather configuration
;; Sunshine weather configuration
(defun my-kelvin-to-fahrenheit (kelvin)
  "Convert Kelvin to Fahrenheit."
  (- (* kelvin 1.8) 459.67))
(defun my-weather-display ()
  "Display weather for Athens, NY."
  (interactive)
  (let ((url (format "https://api.openweathermap.org/data/2.5/weather?q=Athens,NY,US&appid=%s"
                     (get-env-or-warn "OPENWEATHER_API_KEY"))))
    (url-retrieve url
                  (lambda (_status)
                    (goto-char url-http-end-of-headers)
                    (let* ((json-object-type 'hash-table)
                           (json-array-type 'list)
                           (json-key-type 'string)
                           (data (json-read))
                           (temp (gethash "temp" (gethash "main" data)))
                           (desc (gethash "description" (car (gethash "weather" data))))
                           (humidity (gethash "humidity" (gethash "main" data))))
                      (with-current-buffer (get-buffer-create "*Weather*")
                        (erase-buffer)
                        (insert (format "Weather in Athens, NY:\n\n"))
                        (insert (format "Temperature: %.1f°F\n" (my-kelvin-to-fahrenheit temp)))
                        (insert (format "Conditions: %s\n" desc))
                        (insert (format "Humidity: %d%%\n" humidity))
                        (display-buffer (current-buffer))))))))

;; Bind it to a key
(map! :leader
      (:prefix ("w" . "weather")
       :desc "Weather forecast" "w" #'my-weather-display))

;; REMOVED: (load! "goldensunshine")
;;(after! goldensunshine
;;  (setq goldensunshine-api-key (get-env-or-warn "OPENWEATHER_API_KEY")
;;        goldensunshine-location "Athens,NY,US"))  ;; e.g., "Los Angeles, CA"
;;(setq goldensunshine-auto-theme nil)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; MATRIX CLIENT ELEMENT                                             ;;
;; (after! ement                                                        ;;
;;   (map! :leader                                                      ;;
;;         (:prefix ("M" . "matrix")                                    ;;
;;          :desc "Connect to Matrix" "c" #'ement-connect               ;;
;;          :desc "List rooms" "l" #'ement-list-rooms                   ;;
;;          :desc "View room" "r" #'ement-view-room                     ;;
;;          :desc "Send direct message" "d" #'ement-send-direct-message ;;
;;          :desc "Send message" "m" #'ement-room-send-message)))       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; macOS Screensaver Functions
(defun macos-start-screensaver ()
  "Start macOS screensaver."
  (interactive)
  (shell-command "open -a ScreenSaverEngine"))

;; Key binding for the screensaver
(map! :leader
      (:prefix ("s" . "screensaver")
       :desc "Start macOS Screensaver" "f" #'macos-start-screensaver))
;; 
;;   (interactive)

;; Run the autostart setup after EXWM initializes

;; Alternative approach using a timer
(defvar screensaver-timer nil "Timer for screensaver activation.")

(defun check-idle-time ()
  "Check if idle time exceeds threshold and run screensaver if it does."
  (when (> (float-time (current-idle-time)) 600) ; 600 seconds = 10 minutes
    (macos-start-screensaver)))
;;
;;
;;
;; =============================================================================
;; RASPBERRY PI PERFORMANCE OPTIMIZATIONS - Add this block
;; =============================================================================

;; Fix org-persist performance disaster
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (after! org                                              ;;
;;   (setq org-persist-disable-when-emacs-Q t               ;;
;;         org-persist-directory nil                        ;;
;;         (advice-add 'org-persist-read :override #'ignore))     ;;
;;                                                          ;;
;; ;; Fix URL cookie performance disaster                   ;;
;; (setq url-cookie-file nil                                ;;
;;       url-cookie-save-interval nil)                      ;;
;; (advice-add 'url-cookie-write-file :override #'ignore)   ;;
;;                                                          ;;
;; ;; Reduce display-time frequency                         ;;
;; (setq display-time-interval 60)                          ;;
;;                                                          ;;
;; ;; Reduce flyspell aggressiveness                        ;;
;; (after! flyspell                                         ;;
;;   (setq flyspell-lazy-idle-seconds 5                     ;;
;;         flyspell-lazy-window-idle-seconds 10))           ;;
;;                                                          ;;
;; ;; Reduce other timer frequencies                        ;;
;; (after! dmenu                                            ;;
;;   (setq dmenu-cache-executable-files-delay 300))         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Also make sure this line is commented out:
;; (run-with-idle-timer 30 t #'save-some-buffers)

;;(defun start-screensaver-timer ()
;;  "Start the timer to check for idle time."
;;  (interactive)
;;  (when screensaver-timer
;;    (cancel-timer screensaver-timer))
;;  (setq screensaver-timer (run-with-idle-timer 600 t #'macos-start-screensaver)))
;;
;; Safe reboot/shutdown functions for macOS
(defun macos-restart ()
  "Safely save Emacs state and restart macOS."
  (interactive)
  (when (yes-or-no-p "Really restart macOS? ")
    (save-some-buffers t)  ; Save all modified buffers
    (recentf-save-list)    ; Save recent files
    (shell-command "osascript -e 'tell app \"System Events\" to restart'")))

(defun macos-shutdown ()
  "Safely save Emacs state and shut down macOS."
  (interactive)
  (when (yes-or-no-p "Really shut down macOS? ")
    (save-some-buffers t)
    (recentf-save-list)
    (shell-command "osascript -e 'tell app \"System Events\" to shut down'")))

;; Bind to convenient keys
;;; ============================================
;;; GARDEN CALENDAR CONFIGURATION - BIDIRECTIONAL SYNC
;;; ============================================

;; CRITICAL: plstore configuration for OAuth tokens
(setq plstore-cache-passphrase-for-symmetric-encryption t)
(setq epg-pinentry-mode 'loopback)

;; Set org agenda files to include garden calendar
(after! org
  (setq org-agenda-files '("~/cloud/org/gardening/garden.org"
                           "~/cloud/org/todo.org"
                           "~/cloud/org/gardencalendar.org"))

  ;; Agenda configuration
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1) ;; Start on Monday
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t))

;; Google Calendar sync (org-gcal) - BIDIRECTIONAL
;; Set credentials BEFORE loading org-gcal to avoid warnings on startup
(setq org-gcal-client-id (get-env-or-warn "GOOGLE_CALENDAR_CLIENT_ID")
      org-gcal-client-secret (get-env-or-warn "GOOGLE_CALENDAR_CLIENT_SECRET")
      ;; ALL THREE FILES NOW SYNC TO GOOGLE CALENDAR
      org-gcal-fetch-file-alist '(("forrestmuelrath@gmail.com" . "~/cloud/org/gardencalendar.org")
                                   ("forrestmuelrath@gmail.com" . "~/cloud/org/todo.org")
                                   ("forrestmuelrath@gmail.com" . "~/cloud/org/diary2025.org")))

(use-package! org-gcal
  :config

  ;; Optional: Set your timezone if different from system
  ;; (setq org-gcal-local-timezone "America/New_York")

  ;; Optional: Configure recurring events display
  (setq org-gcal-recurring-events-mode 'top-level)

  ;; Optional: Don't notify on every sync
  (setq org-gcal-notify-p nil))

;; Visual calendar (calfw)
(use-package! calfw
  :config
  (setq calendar-week-start-day 1)) ;; Start week on Monday

(use-package! calfw-org
  :after calfw)

;; Custom calendar functions - UPDATED FOR BIDIRECTIONAL SYNC
(defun garden/sync-calendar ()
  "BIDIRECTIONAL sync: Fetch from Google Calendar AND push local changes"
  (interactive)
  (org-gcal-sync))  ;; This does BOTH fetch and push

(defun garden/fetch-only ()
  "Fetch from Google Calendar only (don't push changes)"
  (interactive)
  (org-gcal-fetch))

(defun garden/push-current-event ()
  "Push the event at point to Google Calendar"
  (interactive)
  (org-gcal-post-at-point))

(defun garden/open-calendar ()
  "Open calendar view with org events"
  (interactive)
  (cfw:open-org-calendar))

;; Keybindings for garden calendar - ENHANCED
(map! :leader
      (:prefix ("g" . "garden")
       :desc "Open visual calendar" "c" #'garden/open-calendar
       :desc "Bidirectional sync (fetch+push)" "s" #'garden/sync-calendar
       :desc "Fetch only (no push)" "f" #'garden/fetch-only
       :desc "Push event at point" "p" #'garden/push-current-event
       :desc "Open garden.org" "g" (lambda () (interactive) (find-file "~/cloud/org/gardening/garden.org"))
       :desc "Open gardencalendar.org" "a" (lambda () (interactive) (find-file "~/cloud/org/gardencalendar.org"))
       :desc "Open todo.org" "t" (lambda () (interactive) (find-file "~/cloud/org/todo.org"))
       :desc "Open diary.org" "d" (lambda () (interactive) (find-file "~/cloud/org/diary2025.org"))))

;; Video playback functions
(defun my/play-falling-snowflakes ()
  "Play falling snowflakes video fullscreen on loop."
  (interactive)
  (start-process "mpv" nil "mpv" "--fs" "--loop" "~/Videos/fallingsnowflakes.mp4"))

(map! :leader
      (:prefix ("v" . "video")
       :desc "Play snowflakes" "s" #'my/play-falling-snowflakes))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;________        _     ___      ______      _____________ ________
;;`MMMMMMMb.     dM.    `MM\     `M'`MM\     `M'`MMMMMMMMM `MMMMMMMb.
;; MM    `Mb    ,MMb     MMM\     M  MMM\     M  MM      \  MM    `Mb
;; MM     MM    d'YM.    M\MM\    M  M\MM\    M  MM         MM     MM
;; MM    .M9   ,P `Mb    M \MM\   M  M  \MM\  M  MM    ,    MM     MM
;; MMMMMMM(    d'  YM.   M  \MM\  M  M  \MM\  M  MMMMMMM    MM    .M9
;; MM    `Mb  ,P   `Mb   M   \MM\ M  M   \MM\ M  MM    `    MMMMMMM9'
;; MM     MM  d'    YM.  M    \MM\M  M    \MM\M  MM         MM  \M\
;; MM     MM ,MMMMMMMMb  M     \MMM  M     \MMM  MM         MM   \M\
;; MM    .M9 d'      YM. M      \MM  M      \MM  MM      /  MM    \M\
;;_MMMMMMM9_dM_     _dMM_M_      \M _M_      \M _MMMMMMMMM _MM_    \M\_
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-weebery-is-always-greater ()
  (let* ((banner '("
                   *    .   * .    *   .    *     *   .    *  *  .
     .       *  .    *  *   .    *   ** '   .      *         .         .
                  *  .    *       *  .     *   .   *  .    *
            .    *   .    *    .   *    ** '   .      *         *    .
       '    *    .   *  .   '    *   .    *   .   *
           .    *   .    *      *    .   *     *      * '   .      *         .
                 .    *   *  .    *  *   .    *   *
                  *  .    *       *  .     *   .   *  .    *
             .   * * '      .      *         .  *        *
                          .    * '   .     .    *    .         *
            .        *  .    *   .    *     *   .    *    .

                     * .' *    `   .      *  ,   *  *  .    *  *
         .    * . ' * . ' ., * '
           * '   .      *         .  *        *    .       .
            . .    *      *    .   *     *    *     .    *    .
       *    *   .   *     .    *    .     *.     *    .    *   .
                   *    .   * .    *   .    *     *   .    *  *  .
     .       *  .    *  *   .    *   ** '   .      *         .         .
                  *  .    *       *  .     *   .   *  .    *
            .    *   .    *    .   *    ** '   .      *                *    .
                    * .' *    `   .      *  ,   *  *  .    *  *
         .    * . ' * . ' ., * '
           * '   .      *         .  *        *    .       .
            . .    *      *    .   *     *    *     .    *    .
       *    *   .   *     .    *    .     *.     *    .    *   .
                   *    .   * .    *   .    *     *   .    *  *  .
     .       *  .    *  *   .    *   ** '   .      *         .         .
                  *  .    *       *  .     *   .   *  .    *
            .    *   .    *    .   *    ** '   .      *                *    .
       '    *    .   *  .   '    *   .    *   .   *
           .    *   .    *      *    .   *     *      * '   .      *         .
                 .    *   *  .    *  *   .    *   *
                  *  .    *       *  .     *   .   *  .    *
    .   * * '   .      *         .  *        *    .    * '   .     .    *    .
            .        *  .    *   .    *     *   .    *    .

                                Forrest'S COMPUTER
                                    (keep out)
"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))


     'face 'doom-dashboard-banner)))
(setq +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)

;; Custom dashboard buttons - the RIGHT way
;; Using official Doom guide approach
;; CORRECTED DASHBOARD BUTTONS - This version WORKS
;; The problem: make-text-button inside concat doesn't work
;; The fix: Use insert-text-button DIRECTLY in the buffer

;; DELETE THE OLD VERSION and REPLACE with this:

;; Custom dashboard buttons - WORKING VERSION
;; The key: insert buttons INDIVIDUALLY, not in concat
(add-hook! '+doom-dashboard-functions :append
  (insert "\n")
  (let ((start (point)))
    ;; Browser button
    (insert-text-button "🌐 Browser"
                        'action (lambda (_) (my/open-chrome))
                        'face 'doom-dashboard-menu-title
                        'help-echo "Open Chrome")
    (insert "    ")

    ;; Terminal button
    (insert-text-button "📟 Terminal"
                        'action (lambda (_) (my/open-terminal))
                        'face 'doom-dashboard-menu-title
                        'help-echo "Open Terminal")
    (insert "    ")

    ;; Diary button
    (insert-text-button "✎ Diary"
                        'action (lambda (_) (find-file "~/cloud/org/main/diary2025.org"))
                        'face 'doom-dashboard-menu-title
                        'help-echo "Open diary")

    ;; Center the whole line
    (let ((line-text (buffer-substring start (point))))
      (delete-region start (point))
      (insert (+doom-dashboard--center +doom-dashboard--width line-text)))))

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width
                                        "emacsOS - Powered by Emacs & Lisp")))

;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; SuperCollider setup
(setenv "PATH" (concat "/Applications/SuperCollider.app/Contents/MacOS:" (getenv "PATH")))
(setq exec-path (cons "/Applications/SuperCollider.app/Contents/MacOS" exec-path))

(after! sclang
  (setq sclang-program "/Applications/SuperCollider.app/Contents/MacOS/sclang")
  (setq sclang-runtime-directory "/Applications/SuperCollider.app/Contents/Resources/")
  (setq sclang-library-configuration-file
        (expand-file-name "~/.config/SuperCollider/sclang_conf.yaml"))
  (setq sclang-help-path '("/Applications/SuperCollider.app/Contents/Resources/HelpSource"))

  ;; Ensure post buffer is visible
  (setq sclang-post-buffer "*SCLang:PostBuffer*")
  (setq sclang-show-workspace-on-startup nil)

  ;; Auto-show post buffer after starting sclang
  (advice-add 'sclang-start :after
              (lambda (&rest _)
                (when (get-buffer "*SCLang:PostBuffer*")
                  (display-buffer "*SCLang:PostBuffer*"))))

  ;; Helper functions to boot/quit server
  (defun sclang-boot-server ()
    "Boot the SuperCollider default server."
    (interactive)
    (sclang-eval-string "Server.default.boot;"))

  (defun sclang-quit-server ()
    "Quit the SuperCollider default server."
    (interactive)
    (sclang-eval-string "Server.default.quit;"))

  (defun sclang-reboot-server ()
    "Reboot the SuperCollider default server."
    (interactive)
    (sclang-eval-string "Server.default.reboot;"))

  ;; Key bindings
  (map! :map sclang-mode-map
        :localleader
        :desc "Start SCLang" "s" #'sclang-start
        :desc "Stop SCLang" "q" #'sclang-stop
        :desc "Recompile" "c" #'sclang-recompile
        :desc "Eval line" "e" #'sclang-eval-line
        :desc "Eval region" "r" #'sclang-eval-region
        :desc "Eval defun" "d" #'sclang-eval-defun
        :desc "Show post buffer" "p" #'sclang-show-post-buffer
        :desc "Clear post buffer" "C" #'sclang-clear-post-buffer
        :desc "Boot server" "b" #'sclang-boot-server
        :desc "Quit server" "k" #'sclang-quit-server
        :desc "Reboot server" "R" #'sclang-reboot-server)

  ;; Use define-key for direct keybindings (more reliable than map!)
  (define-key sclang-mode-map (kbd "C-c C-c") 'sclang-eval-line)
  (define-key sclang-mode-map (kbd "C-c C-e") 'sclang-eval-region)
  (define-key sclang-mode-map (kbd "C-M-x") 'sclang-eval-defun)
  (define-key sclang-mode-map (kbd "C-c C-b") 'sclang-boot-server)
  (define-key sclang-mode-map (kbd "C-c C-y") 'sclang-help-gui)

  ;; NOTE: Disabled company-mode - using Corfu instead
  ;; SuperCollider-specific gptel configuration (without enabling gptel-mode)
  ;; gptel-mode is not compatible with sclang-mode, but you can still use gptel functions
  (add-hook 'sclang-mode-hook
            (lambda ()
              ;; Only set gptel config if gptel is loaded
              (when (featurep 'gptel)
                (setq-local gptel-directives
                            '((default . "You are a SuperCollider expert. Help me write, debug, and optimize sclang code. Follow best practices for synthesis, patterns, and server communication.")
                              (programming . "Focus on sclang syntax, UGen usage, and efficient server patterns."))
                  gptel-model 'claude-haiku-4-5-20251001
                  gptel-backend (gptel-get-backend "Claude-Haiku-4.5")))))

  ;; (add-hook 'sclang-mode-hook 'company-mode)
  ;; (add-to-list 'company-backends 'company-sc)
  )