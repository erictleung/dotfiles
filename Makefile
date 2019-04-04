# set shell
SHELL := /usr/bin/env bash

## vundle : Install Vundle for managing Vim packages
vundle :
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall

## dirs : Setup directory structure
dirs :
	mkdir -p ../{documents,downloads,misc}

## ssh : Get SSH key for easy copy-paste
ssh :
	echo ""

## conda : Install conda for Python package management
# Add back "_64" for 64-bit systems
# Note: doesn't quite work yet
conda :
	curl \
		-o ~/downloads/miniconda.sh \
		https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86.sh
	bash ~/downloads/miniconda.sh

.PHONY : help example conda dirs vundle

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
