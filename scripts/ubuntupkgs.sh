#!/usr/bin/env bash

# Update package listing
sudo apt-get update
sudo apt-get upgrade

# Install Ubuntu packages
sudo apt-get install \
    tmux \ # Terminal multiplexer
    htop ncdu \ # Nice GUI replacements
    diction aspell \ # Help with writing
    vim emacs \ # Editors
    pandoc pandoc-citeproc \ # Pandoc and tools
    stow \ # Configuration management
    w3m \ # Command line web browser
    tree # Misc tools
