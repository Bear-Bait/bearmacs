# Org-roam Installation and Configuration Guide for Doom Emacs

This document outlines the steps taken to install and configure `org-roam` and `org-roam-ui` in your Doom Emacs setup, ensuring safe operation across multiple machines with a cloud-synced `org` directory.

## Multi-Machine Setup

The `~/cloud/org/` directory is shared via Nextcloud across three machines running Emacs:
- **Mac Mini** (macOS)
- **Raspberry Pi** (Linux)
- **MacBook** (macOS)

All `.org` note files live in the cloud-synced directory. Each machine maintains its own local org-roam SQLite database (see `org-roam-db-location` below) to avoid database corruption from concurrent cloud sync writes. The `packages.el`, `config.el`, and template files in `~/.doom.d/` must be set up independently on each machine.

## 1. Prerequisites

`org-roam` requires a C/C++ compiler to build its SQLite database tool.

### macOS

*   **Install Xcode Command Line Tools:**
    ```bash
    xcode-select --install
    ```
    Follow the prompts to complete the installation.

### Linux (Raspberry Pi / Debian-based)

*   **Install build tools and SQLite:**
    ```bash
    sudo apt update
    sudo apt install gcc make libsqlite3-dev sqlite3
    ```

## 2. Modify `packages.el`

This step adds the `org-roam` and `org-roam-ui` packages to your Doom Emacs `packages.el` file, signaling Doom to install them.

*   **Open `~/.doom.d/packages.el`**
*   **Add the following lines** to the "General packages" section (or any suitable location):

    ```elisp
    (package! org-roam)
    (package! org-roam-ui)
    ```

    *Self-Correction Example (Contextual):* Your `packages.el` already had `(package! evil-tutor)`. The additions were placed before it. The final section should look similar to this (exact order might vary):
    ```elisp
    ;; General packages
    (package! org-roam)
    (package! org-roam-ui)
    (package! evil-tutor)
    ;; ... other packages
    ```

## 3. Modify `config.el`

This step configures `org-roam`, `org-roam-dailies`, and `org-roam-ui`, with a critical setup for cloud syncing safety.

*   **Open `~/.doom.d/config.el`**
*   **Locate and replace the `org-roam` configuration block.**
    The existing commented-out block for `org-roam` was updated. Replace that entire section with the following (or add it if you didn't have a commented block):

    ```elisp
    (use-package! org-roam
      :init
      (setq org-roam-v2-ack t) ; Acknowledge Org Roam v2
      :custom
      (org-roam-directory "~/cloud/org/roam") ; Set your desired directory for notes
      ;; CRITICAL FOR CLOUD SYNCING: Store database locally, NOT in cloud sync
      (org-roam-db-location "~/.doom.d/.local/org-roam-db/") ; Set local directory for the database
      (org-roam-completion-everywhere t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n i" . org-roam-node-insert))
      :config
      ;; Ensure the local database directory exists before org-roam tries to use it
      (unless (file-exists-p org-roam-db-location)
        (make-directory org-roam-db-location t))
      (org-roam-db-autosync-mode))

    ;; Daily notes configuration for org-roam
    (use-package! org-roam-dailies
      :after org-roam
      :config
      (setq org-roam-dailies-directory "daily/") ; Daily notes will be in ~/cloud/org/roam/daily/
      (setq org-roam-dailies-capture-templates
            '(("d" "default" entry
               "* %?"
               :target (file+head "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>
")))))

    ;; Org-roam-ui configuration
    (use-package! org-roam-ui
      :after org-roam
      :config
      (setq org-roam-ui-browser-function 'eww-browse-url) ; Use eww for internal browser
      (setq org-roam-ui-open-on-start t)) ; Open UI on Emacs start
    ```

### Toolkit Unit Test Capture Template

This adds a dedicated capture template for Wave Farm Internet Radio Toolkit testing. Add this inside your `(after! org ...)` block in `config.el`:

```elisp
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
           :unnarrowed t))))
```

The template file `~/.doom.d/templates/toolkit-unit-test.org` contains the 6-phase testing protocol (hardware inspection, WiFi, transmitting, receiving, pots, mic gain).

**Usage:**
1. `M-x org-roam-capture`
2. Type a node name (e.g., "unit-1")
3. Press Enter
4. Select **u** for Toolkit Unit Test
5. Fill in the template, then `C-c C-c` to save

**Important:** Nodes require a `:PROPERTIES:` drawer with `:ID:` to be recognized by org-roam. The capture template generates this automatically. If you create files manually, use `M-x org-id-get-create` inside the file to add the ID.

### Explanation of Key Configuration:

*   **`org-roam-directory "~/cloud/org/roam"`**: This specifies the directory where your actual `.org` note files are stored. This folder is expected to be managed by your cloud syncing service (e.g., Nextcloud).
*   **`org-roam-db-location "~/.doom.d/.local/org-roam-db/"`**: **This is crucial for multi-machine setups.** It tells `org-roam` to store its SQLite database (`org-roam.db`) in a *local* directory on each machine, outside of your cloud-synced folder.
    *   **Why?** SQLite databases are not designed for concurrent writes across cloud-synced directories, which can lead to corruption. By keeping the database local to each Emacs instance, you avoid this risk.
    *   Each Emacs instance will maintain its own index of your notes. `org-roam-db-autosync-mode` (enabled in the config) will automatically update this local database when changes are detected in your `org-roam-directory`. If you make changes on another machine and they sync via Nextcloud, the local database on the current machine will rebuild to reflect those changes.

*   **Keybindings:**
    *   `C-c n l`: Toggle `org-roam-buffer` (shows current node's backlinks and linked references).
    *   `C-c n f`: Find (or create) an `org-roam` node.
    *   `C-c n i`: Insert a link to an `org-roam` node.
    *   These are standard bindings recommended by System Crafters tutorials.

## 4. Finalizing the Installation

After making the changes to `packages.el` and `config.el`:

1.  **Run `doom sync` in your terminal:**
    ```bash
    doom sync
    ```
    This command will install `org-roam` and `org-roam-ui`, along with their dependencies, and apply your configuration.
2.  **Restart Emacs.**

Upon restarting Emacs, `org-roam` should be active, and `org-roam-ui` should attempt to open in `eww`. You can start creating and linking notes using the configured keybindings. Remember to repeat these steps on all your machines.
