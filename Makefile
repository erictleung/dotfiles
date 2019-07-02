# set shell
SHELL := /usr/bin/env bash

## vundle : Install Vundle for managing Vim packages
vundle :
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall

## dirs : Setup directory structure
dirs :
	mkdir -p ../{documents,downloads,misc}

## ssh : Get SSH key for easy copy-paste (WIP)
ssh :
	echo ""

## conda : Install conda for Python package management
# Remove "_64" for 32-bit systems
# Rename directory from Miniconda3/ to conda/
conda :
	curl \
		-o ~/downloads/miniconda.sh \
		https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
	cd ~; bash ~/downloads/miniconda.sh
	rm ~/downloads/miniconda.sh

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
aptclean :
	sudo apt autoclean
	sudo apt-get clean
	sudo apt autoremove

## npmclean : Clear npm cache
# Source: https://docs.npmjs.com/cli/cache
npmclean :
	sudo npm cache clean --force
	sudo npm cache verify

## setup : General setup for new environments
setup :
	stow vim
	stow emacs
	stow git
	stow bash

.PHONY : help example conda dirs vundle cclean ssh setup

## help : Help page for Makefile
help :
	@echo ""
	@echo "Usage:"
	@echo -e "\tmake <target>\n"
	@echo -e "Target\t\tDescription"
	@echo -e "------\t\t-----------"
	@grep '## [A-Za-z]* : [A-Za-z]*' $(MAKEFILE_LIST) | \
		sed 's/## //' | \
		awk -F" : " '{ printf "%-13s%s\n", $$1, $$2}'

.DEFAULT_GOAL := help
