#!/usr/bin/env bash

# Update package listing
sudo apt-get update
sudo apt-get upgrade

# Install Ubuntu packages
sudo apt-get install \
    # GUI replacements
    htop \              # Nicer top
    ncdu \              # Nicer du
    dfc \               # Nicer df

    tmux \              # Terminal multiplexer
    vim emacs \         # Editors
    stow \              # Configuration management
    w3m \               # Command line web browser

    # Document writing
    style \
    diction \
    aspell \            # Help with writing
    pandoc \
    pandoc-citeproc \   # Pandoc and tools

    # Misc tools
    tree
