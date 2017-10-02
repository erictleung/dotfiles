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
