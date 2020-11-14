#!/usr/bin/env bash

# Setup by changing folders
# https://stackoverflow.com/a/23889603/6873133
npm config set prefix ~/.npm
export PATH="$PATH:$HOME/.npm/bin"

# npm packages
npm install -g \
    # File diff between pandoc files
    pandiff \

    # Better diffs in git
    diff-so-fancy \

    # Generate table of contents for Markdown files
    doctoc
