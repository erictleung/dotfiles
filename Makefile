# set shell
SHELL := /usr/bin/env bash

## dirs : Setup directory structure
dirs :
	mkdir -p ../{documents,downloads,misc}
	mkdir -p ../documents/{gitlab,github}

## ssh : Get SSH key for easy copy-paste
ssh : ~/.ssh/id_rsa.pub
	cat ~/.ssh/id_rsa.pub

## makessh : Generate local SSH key
makessh :
	ssh-keygen

## conda : Install conda for Python package management
# Remove "_64" for 32-bit systems
# Rename directory from Miniconda3/ to conda/
CONDA=~/documents/miniconda.sh
conda :
	curl \
		-o $(CONDA) \
		https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
	cd ~; bash $(CONDA)
	rm $(CONDA)

## cclean : Clean conda packages and cache
# See conda clean --help for more
cclean :
	conda clean --all

## archclean : Clean Arch Linux cache
# Source:
# https://www.ostechnix.com/recommended-way-clean-package-cache-arch-linux/
archclean :
	sudo paccache -r

## aptclean : Clean apt-get computers
# Source:
# https://askubuntu.com/a/32224
aptclean :
	sudo apt autoclean
	sudo apt-get clean
	sudo apt autoremove

## npmclean : Clear npm cache
# Source: https://docs.npmjs.com/cli/cache
npmclean :
	sudo npm cache clean --force
	sudo npm cache verify

## pipclean : Clear pip cache
# Source: https://stackoverflow.com/a/61762308/6873133
pipclean :
	pip cache purge

## editors : Speed test editor start-up
# Source: https://emacs.stackexchange.com/q/39484/18898
editors :
	time vim +qall
	time emacs -nw -kill
	time emacs -nw -q -kill

## emacs : Tangle Emacs config
emacs : ~/.emacs.d/README.org
	emacs \
		--batch \
		--eval \
		"(require 'org)" \
		--eval "(org-babel-load-file \"~/.emacs.d/README.org\")"

## emacsconfig : Copy over configuration on Windows
# Windows computer setup has the configuration elsewhere and needs to be moved
# Symbolic linking didn't work.
emacsconfig :
	cp ~/.emacs.d/README.org emacs/.emacs.d/README.org

## rstudio : Install RStudio config in Unix-like systems
rstudio :
	cp rstudio/rstudio-prefs.json ~/.config/rstudio

## rstudio-win : Install RStudio config in Windows
rstudio-win :
	cp rstudio/rstudio-prefs.json ~/AppData/Roaming/RStudio/

## vundle : Install Vundle for managing Vim packages
vundle :
	git clone \
		https://github.com/VundleVim/Vundle.vim.git \
		~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall

## ubuntu : Setup Ubuntu setup
# Setup new Ubuntu setup such as on crouton Chromebook
ubuntu :
	sudo apt-get update
	sudo apt-get upgrade
	sudo apt-get install \
		htop ncdu dfc \
		tmux vim emacs w3m \
		style diction aspell pandoc pandoc-citeproc \
		tree man-db \
		libcurl4-openssl-dev libxml2-dev


## flatpak : Setup Flatpak
# Set up Flatpak applications, usually on Chromebook
flatpak :
	# https://flathub.org/apps/me.kozec.syncthingtk
	flatpak install flathub me.kozec.syncthingtk

	# https://flathub.org/apps/com.visualstudio.code
	flathub install flathub com.visualstudio.code

	# https://flathub.org/apps/org.gimp.GIMP
	flathub install flathub org.gimp.GIMP

	# https://flathub.org/apps/org.libreoffice.LibreOffice
	flathub install flathub org.libreoffice.LibreOffice

	# https://flathub.org/apps/md.obsidian.Obsidian
	flathub install flathub md.obsidian.Obsidian

## homebrew : Setup Homebrew software
# Install useful Homebrew software
homebrew :
	# Basic updates
	brew update
	brew upgrade --all

	# Install command-line tools
	brew install vim --override-system-vi
	brew install git
	brew install pandoc
	brew install pandoc-citeproc

	brew cleanup  # Clean up outdate versions


## npm : Setup npm software
# Install useful npm software
npm :
	# Setup by changing folders
	# https://stackoverflow.com/a/23889603/2468369
	npm config set prefix ~/.npm
	export PATH="$PATH:$HOME/.npm/bin"

	# Install npm packages
	npm install -g \
		# File diff between pandoc files
		pandiff \

		# Better diffs in git
		diff-so-fancy \

		# Generate table of contents for Markdown files
		doctoc


.PHONY : help example conda dirs vundle cclean ssh makessh setup editors emacs \
	ubuntu npmclean pipclean archclean emacsconfig flatpak homebrew npm

## help : Help page for Makefile
help :
	@echo ""
	@echo "Usage:"
	@echo -e "\tmake <target>\n"
	@echo -e "Target       Description"
	@echo -e "------       -----------"
	@grep '## [A-Za-z]* : [A-Za-z]*' $(MAKEFILE_LIST) | \
		sed 's/## //' | \
		awk -F" : " '{ printf "%-13s%s\n", $$1, $$2}'

.DEFAULT_GOAL := help
