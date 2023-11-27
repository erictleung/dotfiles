# dotfiles

A repository of my dotfiles and OS-specific configuration details.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Contents**

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
  - [Conda](#conda)
  - [Ruby](#ruby)
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

If possible, installing the
[Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10)
is the most ideal situation to having a terminal-like interface.

As an Emacs user, I want to use the visual GUI. This requires an X Client to
generate the GUI. This can done using X Launch. Some instructions to set this
up are written up [here](https://sudhakaryblog.wordpress.com/2018/08/30/run-gui-app-on-windows-xlaunch-vcxsrv/).

If WSL cannot used because of administrative privileges, the second best option
is to install an Emacs GUI with [Git Bash](https://gitforwindows.org/). To
install the GUI from [here](https://ftp.gnu.org/gnu/emacs/windows/). Download
one of the installers, like `emacs-27.2-x86_64-installer.exe`,should install
Emacs in `C:/Program Files/`.

When opening Emacs, the home directory is located in
`C:/Users/<user_name>/AppData/Roaming/`. Because I use SyncThing to sync my Org
mode notes, SyncThing should put its folders in this `Roaming/` directory for
path navigation to work as expected. Similarly, my `dotfiles/` should be copied
into this directory.

From Git Bash, you can run the following to link these files because `stow`
don't be able to reach it properly.

```sh
cd
ln -s ~/dotfiles/emacs/.emacs.d ~/AppData/Roaming
```

In the past, I have made use of [Cygwin](http://cygwin.com/) as the terminal
window I use. Updates and package management are done using the Cygwin
executable file. However, it appears [MSYS2](https://www.msys2.org/) is a
better alternative bceause it

> "...provide[s] a build environment for native Windows software and the
> Cygwin-using parts are kept at a minimum."

When setting up MSYS2, the path defaults to the `AppData/Roaming/` directory.
To make it more native with the Window's user home director, navigate to
`~/etc/nsswitch.conf` and make the edit

```
db_home: windows # cygwin desc
```

These changes are summarized
[here](https://aktasfatih.com/setting-up-windows-with-msys2/). The packages in
MSYS2 can be found
[here](https://github.com/msys2/MSYS2-packages) and
[here](https://github.com/msys2-unofficial/MSYS2-packages).

There is a package manager, [chocolatey](https://chocolatey.org/), that I've
heard about and have downloaded, but it is less utilized. I will note here on
using it in the future.

### Unix

[Homebrew](https://brew.sh/) and [Linuxbrew](https://docs.brew.sh/Homebrew-on-Linux)
are both great package managers when you don't have access to root.

**Note**: must have [Ruby](https://www.ruby-lang.org/) installed.

```shell
# macOS
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Linux
sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
```

To [uninstall Homebrew](https://github.com/Homebrew/brew/blob/master/docs/FAQ.md),
run the following:

```shell
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/uninstall)"
```

For LaTeX on Ubuntu,

```shell
cd ~
mkdir texmf
sudo apt-get install xzdec
tlmgr init-usertree
```

### Termux

These tools are missing from the shell, so let's install them here.

```sh
pkg install vim emacs make git curl openssh stow
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

After downloading the `crouton` script from the above link, run the following:

```shell
# Install    Just CLI     Encrypt  Get Ubuntu 18.04 LTS
sudo crouton -t cli-extra -e       -r bionic
```

Here are some tools that need to be added immediately:

```shell
sudo apt-get install build-essential stow git vim make curl nodejs npm \
    syncthing htop tmux
```

Here are some instructions to
[install Emacs 26.1 on Ubuntu](http://ubuntuhandbook.org/index.php/2019/02/install-gnu-emacs-26-1-ubuntu-18-04-16-04-18-10/):

```shell
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs26
echo "alias emacs=emacs26" >> .bash_local
```


## Software Specific Setups

### Emacs Info

Here are some notable packages I regularly depend on:

- `academic-phrases` for academic writing
- `ace-window` for better window management
- `avy/swiper` for navigation in files
- `company` for autocomplete
- `deft`/`zetteldeft` for quick note management
- `elfeed`/`elfeed-org`/`elfeed-goodies` for RSS reading
- `helm-bibtex` for searching and managing bibliographies
- `org-brain` for concept mapping
- `org-mode` for drafting documents and organizing
- `org-ref` for BibTeX bibliography management
- `projectile` for quickly navigating files across project
- `try` for simply trying out packages without commitments
- `w3m` for browsing the internet within Emacs
- `which-key` for hints when pressing key chords
- `writegood-mode` for writing well
- `yasnippet` for quick snippets and templates

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

### Conda

Miniconda and the `conda` package manager are used for general Python software
management.

See https://docs.conda.io/en/latest/miniconda.html for OS-specific
installations.

See
https://docs.anaconda.com/anaconda/user-guide/tasks/using-r-language/
for using R with Anaconda.

### Ruby

To make sure the Ruby gem path is in the path, run the following to see where
the gems are being installed.

```shell
gem environment
```

Source: https://stackoverflow.com/a/19072136/6873133

This environment can also be manually set as well. (May end up doing this to
make things more consistent.)

```shell
export PATH=$PATH:$HOME/.gem/bin
export GEM_HOME=$HOME/.gem
export GEM_PATH=$HOME/.gem
```

Source: https://unix.stackexchange.com/a/210012/265438

## Useful Software

Below are some pieces of software that are useful for working and help me
interface with files and be productive.

**CLI**

- [`pandiff`](https://github.com/davidar/pandiff) - Prose diffs for any
  document format supported by Pandoc; installs by `npm`.
- [`jq`](https://stedolan.github.io/jq/) - Lightweight and flexible
  command-line JSON processor.
- [`ranger`](https://github.com/ranger/ranger) - A VIM-inspired file manager
  for the console.
- [`sqlfluff`](https://github.com/sqlfluff/sqlfluff) - The SQL Linter for
  humans.
- [QRcode.show](https://qrcode.show/) - Generate QR code using curl.
- [ShellCheck](https://github.com/koalaman/shellcheck) - A static analysis
  tool for shell scripts ([live editor](https://www.shellcheck.net/)).

**Web Tools**

- [Tiny Helpers](https://tiny-helpers.dev/) - A collection of free
  single-purpose online tools for web developers.
- [QR Code Generator](https://www.the-qrcode-generator.com/) - Simple and
  useful.
- [repl.it](https://repl.it/) - Create interactive REPLs for lots of languages.
- [JDoodle](https://www.jdoodle.com/) - Like repl.it, but also includes MySQL
  and MongoDB.
- [DB Fiddle](https://www.db-fiddle.com/) - Database-specific fiddle.
- [Online LaTeX editor](https://www.codecogs.com/latex/eqneditor.php) - Quickly
  and easily create LaTeX equation images.
- [Twitter Card Validator](https://cards-dev.twitter.com/validator) - See how
  your website will show up on Twitter.
- [OneLook](https://onelook.com/) - Dictionary with more expressive search
  patterns and also has a nice thesaurus and reverse dictionary.
- [Duckie](http://duckie.me/) - A tool for one person pair programming.
- [remove.bg](https://remove.bg) - Remove image backgrounds.
- [AutoDraw](https://www.autodraw.com/) - Drawing tool with machine learning
  guessing your drawing for quick illustrations.
- [Rentry.co](https://rentry.co/) - Markdown pastebin.
- [doi2bib](https://www.doi2bib.org/) - Take DOI and give BibTeX.
- [diagrams.net](https://app.diagrams.net/) - Easy to create diagrams online
  (formerly draw.io).
- [Resumake](https://resumake.io) - Easy template application to create a
  resume.
- [Quick Database Diagrams](https://quickdatabasediagrams.com/) - Draw
  entity-relationship diagrams by writing pseudocode.
- [Excalidraw](https://github.com/excalidraw/excalidraw) - Virtual whiteboard
  for sketching hand-drawn like diagrams (https://excalidraw.com/).
- [Color Wheel - Canva](https://www.canva.com/colors/color-wheel/) - Tool to
  create good color combinations.
- [Banner](https://liyasthomas.github.io/banner/) - Generate banner images for
  blog posts. Source at https://github.com/liyasthomas/banner.
- [Web Developer Tools](https://www.browserling.com/tools) - Miscellaneous
  convenience tools for web development, the most useful to me so far have been
  the image conversion tools.
- [tinypng](https://tinypng.com/) - Smart PNG and JPEG compression.
- [readme.so](https://readme.so/) - Easily create a README.
- [Online PNG Tools](https://onlinepngtools.com/) - Collection of useful PNG
  image utilities for working with PNG graphics files.
- [sequel fumpt](https://sqlfum.pt/) - Help format SQL with slider width.
- [ConvertCSV](https://www.convertcsv.com/) - Convert CSV files to others.

**Cross-Platform Desktop**

- [Color Oracle](https://colororacle.org/) - Design for color impaired.
- [Freeplane](https://www.freeplane.org/wiki/index.php/Home) - Free mind
  mapping and knowledge management software.
- [Publish or Perish](https://harzing.com/resources/publish-or-perish) -
  Retrieved and analyzes academic citations from variety of sources with a
  number of metrics.
- [TikZit](https://tikzit.github.io/) - Simple GUI editor for graphs and string
  diagrams.
- [VLC Media Player](https://www.videolan.org/vlc/) - Free and open source
  cross-platform multimedia player.
- [LICEcap](https://www.cockos.com/licecap/) - Capture an area of your desktop
  and save it directly to .GIF.

*Note*: The above desktop applications were chosen to be cross platform and
standards based in terms of their inputs and outputs, if applicable.

**Windows Specific**

- [Dexpot](https://www.dexpot.de) - Virtual desktops for Windows.
- [Cygwin](https://www.cygwin.com/) - Linux feeling on Windows 7 (Windows 10
  has Windows Subsystem for Linux (WSL) option for terminal).
- [WinDirStat](https://windirstat.net/) - Disk usage statistics viewer and
  cleanup tool.
- [Rapid Environment Editor](https://www.rapidee.com/en/about) - Windows
  environment variables management.
- [ImageGlass](https://github.com/d2phap/ImageGlass) - A lightweight, versatile
  image viewer.

**Firefox Plugins**

- [Mouseless](https://addons.mozilla.org/en-US/firefox/addon/mouseless-plugin/) - Use
  mouse as least as possible (https://github.com/mortie/mouseless-plugin). See
  `scripts/mouseless.json` file with configuration in JSON form.
- [web-archives](https://addons.mozilla.org/en-US/firefox/addon/view-page-archive) - View
  archived and cached versions of web pages of 10+ search engines
  (https://github.com/dessant/web-archives).
- [Unpaywall](https://github.com/ourresearch/unpaywall-extension) - Gives you a
  link to a free PDF when viewing scholarly articles.
