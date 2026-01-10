;;; eink-newspaper-theme.el --- An e-ink/newspaper theme for writing prose -*- lexical-binding: t; -*-
;;
;; Author: Claude AI
;; Source: Based on doom-opera-light-theme
;;
;;; Commentary:
;;; A minimalist theme emulating e-ink or newspaper for writing prose
;;; Code:

(require 'doom-themes)

(defgroup eink-newspaper-theme nil
  "Options for the `eink-newspaper' theme."
  :group 'doom-themes)

(defcustom eink-newspaper-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'eink-newspaper-theme
  :type '(choice integer boolean))

(def-doom-theme eink-newspaper
  "A minimalist theme emulating e-ink or newspaper for writing prose."

  ;; name        default   256       16
  ((bg         '("#f4f1e8" nil       nil )) ;; Soft, warm off-white
   (bg-alt     '("#e8e5dc" nil       nil ))
   (base0      '("#f4f1e8" nil       nil ))
   (base1      '("#edcedd" nil       nil ))
   (base2      '("#dcd9d0" nil       nil ))
   (base3      '("#d0cdc4" nil       nil ))
   (base4      '("#c4c1b8" nil       nil ))
   (base5      '("#b8b5ac" nil       nil ))
   (base6      '("#aca9a0" nil       nil ))
   (base7      '("#a09d94" nil       nil ))
   (base8      '("#948f86" nil       nil ))
   (fg         '("#303030" nil       nil )) ;; Softer black for main text
   (fg-alt     '("#4a4a4a" nil       nil ))

   (grey       base4)
   (red        '("#8b4b4b" nil       nil )) ;; Muted red
   (orange     '("#9b6a4a" nil       nil )) ;; Muted orange
   (green      '("#dcead7" nil       nil )) ;; Muted green
   (blue       '("#4b4b8b" nil       nil )) ;; Muted blue
   (violet     '("#8b4b8b" nil       nil )) ;; Muted violet
   (highlight  '("#e8d0d0" nil       nil )) ;; Pinkish grey for highlights

   ;; Face categories
   (vertical-bar   (doom-darken base1 0.1))
   (selection      highlight)
   (builtin        fg)
   (comments       base5)
   (doc-comments   base5)
   (constants      fg)
   (functions      fg)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        fg)
   (variables      fg)
   (numbers        fg)
   (region         highlight)
   (error          red)
   (warning        orange)
   (success        green)

   ;; Custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when eink-newspaper-padded-modeline
      (if (integerp eink-newspaper-padded-modeline) eink-newspaper-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg bg-alt)
   (modeline-bg-l (doom-darken bg-alt 0.05))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l (doom-darken bg-alt 0.15)))

  ;; Base theme face overrides
  (((font-lock-comment-face &override) :slant 'italic)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground fg)

   ;; Org-mode specific
   (org-level-1 :foreground blue :weight 'bold :height 1.25)
   (org-level-2 :foreground fg :weight 'bold :height 1.1)
   (org-level-3 :foreground violet :weight 'bold)
   (org-level-4 :foreground green :weight 'bold)
   (org-level-5 :foreground fg :weight 'bold)
   (org-level-6 :foreground fg :weight 'bold)
   (org-level-7 :foreground fg :weight 'bold)
   (org-level-8 :foreground fg :weight 'bold)
   (org-link :foreground blue :underline t)
   (org-checkbox :foreground blue :weight 'bold)
   (org-tag :foreground violet :weight 'normal)
   (org-todo :foreground red :weight 'bold)
   (org-done :foreground green :strike-through t)
   (org-headline-done :foreground base5 :strike-through t)
   (org-date :foreground orange)
   (org-special-keyword :foreground orange)
   (org-document-title :foreground blue :weight 'bold :height 1.5)
   (org-document-info :foreground blue)
   (org-code :foreground orange)
   (org-verbatim :foreground green)
   (org-quote :foreground base7 :slant 'italic)
   (org-verse :inherit 'org-quote)
   (org-warning :foreground red)
   (org-agenda-date :foreground blue)
   (org-agenda-date-today :foreground blue :weight 'bold)
   (org-agenda-done :foreground base5 :strike-through t)
   (org-scheduled :foreground green)
   (org-scheduled-today :foreground green :weight 'bold)
   (org-scheduled-previously :foreground orange)
   (org-upcoming-deadline :foreground red)
   (org-table :foreground fg)
   (org-formula :foreground violet)
   (org-block :background bg-alt :extend t)
   (org-block-begin-line :foreground base5 :background bg-alt :extend t)
   (org-block-end-line :foreground base5 :background bg-alt :extend t)
   (org-meta-line :foreground base5)

   ;; Flyspell
   (flyspell-incorrect :underline `(:style wave :color ,red))
   (flyspell-duplicate :underline `(:style wave :color ,orange))

   ;; Highlight line (for org-mode and others)
   (hl-line :background highlight))

  ;; Additional specifications
  ()
)

;;; eink-newspaper-theme.el ends here
