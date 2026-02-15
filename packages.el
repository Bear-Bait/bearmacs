; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages-macos.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;; General packages
(package! org-roam)
(package! org-roam-ui)
(package! evil-tutor)
;; REMOVED: exwm is Linux-only (X11 window manager)
(package! writegood-mode)
(package! langtool)
(package! wordnut)
(package! synosaurus)
(package! gnus)
(package! claude-shell  ;; Paid $5.00 on July 2, 2024
  :recipe (:host github :repo "arminfriedl/claude-shell"
          :files ("claude-shell.el" "claude-shell-fontifier.el")))

;; Writing tools and thesauri
(package! mw-thesaurus)
(package! org-wc)
(package! dictionary)
(package! artbollocks-mode)
(package! auctex)
(package! auctex-latexmk)
(package! pdf-tools)
(package! org-pdftools)
(package! diffpdf)
(package! pdfgrep)
(package! pdf-view-restore)
(package! wc-mode)
(package! undo-tree)
(package! jinx)
(package! writeroom-mode)

;; File convertors and readers
(package! nov)
;;(package! calibredb)
(package! pandoc)
(package! pandoc-mode)
(package! poet-theme)
(package! ox-epub)
(package! neotree)

;; Scratch buffer and related packages
(package! persistent-scratch)
(package! org-super-agenda)
(package! markdown-preview-eww)
;;(package! tidal)
;;(package! emms)

;; ChatGPT and related tools
;;(package! chatgpt
;;  :recipe (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))
(package! gptel)
(package! chatgpt-shell)

;; Mediawiki
(package! mediawiki)
(package! ox-mediawiki)

;; CSound
(package! csound-mode)

;; SuperCollider
(package! scel
  :recipe (:host github :repo "supercollider/scel"
           :files ("el/*.el")))

;; Web development and media
(package! skewer-mode)
(package! simple-httpd)
(package! js2-mode)
(package! nov-xwidget
  :recipe (:host github :repo "chenyanming/nov-xwidget"))
(package! olivetti)
(package! chess)
;;(package! twittering-mode)
;;(package! bongo)
(package! ac-ispell)
(package! helm-ispell)
(package! wordel)
(package! google-this)
(package! google-maps)
;; (package! vterm)
(package! ready-player)
(package! ps-print)
(package! md4rd)
(package! reddigg)
;;recipe (:host github :repo "xenodium/ready-player"))

;; REMOVED: dmenu is Linux-only (X11 application launcher)
(package! unfill)
(package! impatient-mode)
(package! ellama)
(package! llm)
(package! magit)
;;(package! mu4e-alert)

;; completion adventure EOY 2024
(package! vertico)
(package! orderless)  ; for better fuzzy matching
(package! consult)    ; enhanced search and navigation commands
(package! marginalia) ; adds helpful annotations in the minibuffer
(package! embark)     ; context-aware command menu
(package! emacsql-sqlite3)
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("aider.el" "aider-doom.el")))
;;(package! ement)
(package! sunshine)
(package! aider)
(package! noaa)
(package! languagetool)
(package! flymake-languagetool)
(package! flycheck-languagetool)
(package! ox-hugo)
(package! lexic)
(package! quick-sdcv)

;; Garden Calendar - Google Calendar sync
(package! org-caldav)
(package! calfw)
(package! calfw-org)
(package! org-gcal)
