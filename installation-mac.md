# Doom Emacs Installation Guide for Mac

This document describes how to replicate this Doom Emacs installation on another Mac.

## Current Installation Details

**Emacs Installation:**
- **emacs-plus@30** (version 30.2) from the `d12frosted/emacs-plus` Homebrew tap
- Built with the `--with-cacodemon-icon` option
- Location: `/opt/homebrew/opt/emacs-plus@30/`
- Emacs.app is symlinked to `/Applications/`

**Doom Emacs:**
- Main framework installed at `~/.emacs.d/`
- Custom configuration at `~/.doom.d/`

## Installation Steps

### 1. Install Homebrew (if needed)

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### 2. Install Emacs Plus

```bash
brew tap d12frosted/emacs-plus
brew install emacs-plus@30 --with-cacodemon-icon
```

### 3. Link to Applications

```bash
ln -s /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications/Emacs.app
```

### 4. Install Doom Emacs

```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

### 5. Copy Your Configuration

Copy the entire `~/.doom.d/` directory from this Mac to the new Mac, then run:

```bash
~/.emacs.d/bin/doom sync
```

## Notes

- You may also need to install any system dependencies that your packages require
- If Emacs fails to start with "Library not loaded" errors after upgrading dependencies, reinstall with: `brew reinstall emacs-plus@30`
- Make sure to add `~/.emacs.d/bin` to your PATH for easy access to the doom command
