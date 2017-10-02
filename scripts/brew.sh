#!/usr/bin/env bash

# Install command-line tools using Homebrew.

brew update # Update brew
brew upgrade --all # Upgrade installed

# Install command-line tools
brew install vim --override-system-vi
brew install git
brew install pandoc
brew install pandoc-citeproc

brew cleanup # Clean up outdated versions
