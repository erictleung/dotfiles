# dotfiles

A repository of my dotfiles

## Management

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
# Install dotfiles
cd ~
git clone git@github.com:erictleung/dotfiles.git
cd dotfiles

# Install specific configurations
stow bash # e.g. bash configuration files
```

## Homebrew and Linuxbrew

[Homebrew](https://brew.sh/) and [Linuxbrew](http://linuxbrew.sh/) are both
great package managers when you don't have access to root.

**Note**: must have [Ruby](https://www.ruby-lang.org/) installed.

```shell
# For macOS
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# For Linux
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

## Vim Setup

Run the following.

```
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
```
