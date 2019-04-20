# dotfiles

A repository of my dotfiles and OS-specific configuration details.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Motivation](#motivation)
- [Setup](#setup)
- [Installation](#installation)
- [Operating Systems](#operating-systems)
  - [Windows](#windows)
  - [Unix](#unix)
  - [Termux](#termux)
  - [Chromebook Crouton](#chromebook-crouton)
- [Software Specific Setups](#software-specific-setups)
  - [Emacs Info](#emacs-info)
  - [Vim Setup](#vim-setup)
  - [Git Setup](#git-setup)
  - [rclone](#rclone)
  - [Conda](#conda)
- [Useful Software](#useful-software)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Motivation

This directory, as with many dotfile repositories, aim to create a reproducible
configuration setup for when you move to a new computer. Because of my work and
personal computer usage, I am to account for all major operating systems i.e.
macOS, Windows, and Unix.

[GNU Stow](https://www.gnu.org/software/stow/) is used to manage my dotfiles.
It symbolically links my configuration files to their appropriate places.

## Setup

Install with respect to working operating system.

```shell
sudo apt-get install stow
brew install stow
sudo pacman -S stow
```

## Installation

```shell
# Setup SSH keys
ssh-keygen

# ...Copy key from ~/.ssh/id_rsa.pub
# Add value to GitHub list of acceptable keys

# Install dotfiles
cd ~
git clone git@github.com:erictleung/dotfiles.git
cd dotfiles

# Install specific configurations
stow bash # e.g. bash configuration files
```

## Operating Systems

### Windows

Currently using [Cygwin](http://cygwin.com/) as the terminal window I use.
Updates and package management are done using the Cygwin executable file.

There is a package manager, [chocolatey](https://chocolatey.org/), that I've
heard about and have downloaded, but it is less utilized. I will note here on
using it in the future.

### Unix

[Homebrew](https://brew.sh/) and [Linuxbrew](http://linuxbrew.sh/) are both
great package managers when you don't have access to root.

**Note**: must have [Ruby](https://www.ruby-lang.org/) installed.

```shell
# For macOS
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# For Linux
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

### Termux

These tools are missing from the shell, so let's install them here.

```sh
pkg install vim emacs make git golang curl
```

To install various machine learning packages, I found this repository helpful
in some of the commands to run to install

https://github.com/sanheensethi/Installing-ML-in-Termux-Python

A quick summary of the commands below:

```sh
apt update
apt upgrade

apt install clang git python python-dev fftw freetype freetype-dev libpng \
    libpng-dev pkg-config

pip install --upgrade pip
pip install numpy matplotlib pandas jupyter

apt install wget

curl -o setup.sh https://its-pointless.github.io/setup-pointless-repo.sh
bash setup.sh
apt install scipy
rm setup.sh

pip install scikit-learn
```

### Chromebook Crouton

Not really an operating system, but still distinct from Unix.

Install
[Chromium OS Universal Chroot Environment](https://github.com/dnschneid/crouton).
Instructions are pretty well documented at the link.

The default Ubuntu that installs doesn't come with the latest software (e.g.,
Golang is not up to date). This can cause some issues so be aware.

## Software Specific Setups

### Emacs Info

Here are some notable packages I use:

- `org-mode` for drafting documents and organizing
- `company` for autocomplete
- `academic-phrases` for academic writing
- `yasnippet` for quick snippets and templates
- `elfeed`/`elfeed-org`/`elfeed-goodies` for RSS reading
- `ess` for R and statistics
- `ace-window` for better window management
- `ace-jump-mode` for quick jumping on screen
- `ido` for interatively doing things
- `magit` for an additional interface to git
- `markdown-mode` for editing Markdown files
- `ox-pandoc` for exporting org-mode documents via pandoc
- `try` for simply trying out packages without commitments
- `writegood-mode` for write well
- `pdf-tools` for viewing PDF files within Emacs
- `flycheck` for syntax highlighting
- `which-key` for hints when pressing key chords
- `w3m` for browsing the internet within Emacs
- `org-brain` for concept mapping
- `elpy` for Python programming
- `org-ref` for BibTeX bibliography management
- `interleave` for taking notes with org-mode with the PDF side-by-side
- `helm-bibtex` for searching and managing bibliographies

### Vim Setup

Run the following.

```
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
```

Then from within `vim`, run

```
:PluginInstall
```

to install the listed plugins in `vim` configuration file.

### Git Setup

Reminder that this directory has git configurations that can be done again, but
the information already exists here to make a consistent configuration across
computers.

### rclone

Instead of making use of the Dropbox link through various Linux systems, you
can also install [`rclone`](https://rclone.org/), an rsync for cloud storage.

```bash
# Setup remote cloud storages
rclone config
```

I've named my Dropbox remote as `db`, so I've created an alias that works with
this. Otherwise, another alias will have to be written or just be mindful of
this.

### Conda

Miniconda and the `conda` package manager are used for general Python software
management.

See https://docs.conda.io/en/latest/miniconda.html for OS-specific
installations.

## Useful Software

Below are some pieces of software that are useful for working on the command
line and help me interface with files that are typically used in GUI desktop
versions.

- [`pandiff`](https://github.com/davidar/pandiff) - Prose diffs for any
  document format supported by Pandoc; installs by `npm`.
