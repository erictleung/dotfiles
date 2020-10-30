#!/usr/bin/env bash

# Setup Ubuntu operating system instance
# Tested: Ubuntu 20.04 LTS Focal Fossa
# LTS: Until April 2025

# General directory structure setup
make dirs
make makessh

# Download useful Ubuntu packages
bash scripts/ubuntupkgs.sh

# Setup editors
make vundle
make emacs

# Setup configuration for editors and git
make setup

# Download Python package manager
make conda

# Clean up
make aptclean
