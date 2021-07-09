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

## setup : General setup for new environments
setup :
	stow vim
	stow emacs
	stow git
	stow bash

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
ubuntu : scripts/setup_ubuntu.sh
	bash scripts/setup_ubuntu.sh

.PHONY : help example conda dirs vundle cclean ssh makessh setup editors emacs \
	ubuntu npmclean pipclean archclean

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
