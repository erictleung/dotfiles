# set vim bindings for CLI
# set -o vi

# set default editor to vim
export EDITOR=vim

# colors!
green="\[\033[0;32m\]"
blue="\[\033[0;34m\]"
purple="\[\033[0;35m\]"
reset="\[\033[0m\]"

# change command prompt
source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
# '\u' adds the name of the current user to the prompt
# '\$(__git_ps1)' adds git-related stuff
# '\W' adds the name of the current directory
export PS1="$purple\u$green\$(__git_ps1)$blue \W $ $reset"

# extract - archive extractor
# usage: extract <file>
function extract () {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2)  tar xjf $1;;
            *.tar.gz)   tar xzf $1;;
            *.bz2)      bunzip2 $1;;
            *.rar)      unrar x $1;;
            *.gz)       gunzip $1;;
            *.tar)      tar xf $1;;
            *.tbz2)     tar xjf $1;;
            *.tgz)      tar xzf $1;;
            *.zip)      unzip $1;;
            *.z)        uncompress $1;;
            *.7z)       7z x $1;;
            *)          echo "'$1' cannot be extracted via extract()";;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# ls after cd
# source: https://dev.to/wulfmann/comment/6mp9
function cd {
    OS="$(uname -s)"
    if [[ $OS == "Darwin" ]]; then
        builtin cd "$@" && ls -Gltr
    else
        builtin cd "$@" && ls -Gltr --color
    fi
}

# mkdir and then cd into it
function mcd () {
    mkdir -p $1
    cd $1
}


# find top n edited files in git
# modified from: https://news.ycombinator.com/item?id=16300152
function get_common_edited {
    git log --pretty=format: --name-only |\
        sort |\
        uniq -c |\
        sort -rg |\
        head -${1:-11}
}

# get diff output to be in color
# modified from: https://unix.stackexchange.com/a/444159/
function cdiff {
    if [[ -f "$1" && -f "$2" ]]; then
        diff --color=always -- $1 $2 | less -R
    else
        echo "Needs two files to compare."
    fi
}


# open up Word documents with Vim
function vimd () {
    pandoc -f docx -t rst $1 | vim -
}


# Find and Display SyncThing Conflict Differences
# Take in file name without the extension and compare one of the conflicts.
# Usage: sd gtd
sd () {
    f1=$1.org
    f2=$(ls | grep $1.sync | head -n 1)
    diff $f1 $f2 | less
    echo "Looked at $f2"
}


# allow typos for cd, tab-completion, and better directory management
shopt -s audocd 2> /dev/null
shopt -s dirspell 2> /dev/null
shopt -s cdspell 2> /dev/null

# ignore frequent commands and duplicates
# source: https://askubuntu.com/a/15930
export HISTIGNORE="&:[ ]*:exit:e:R:make:tmux.*:cd:la:ls:gd:gs:c:history:clear"
export HISTCONTROL=ignoredups

# make bash append rather than overwrite the history on disk
shopt -s histappend

# disable beep in less
export LESS="$LESS -R -Q"

# use aliases
if [ -f ~/.aliases ]; then
    source ~/.aliases;
fi

# add export commands
if [ -f ~/.exports ]; then
    source ~/.exports;
fi

# run git completion script if it exists
if [ -f ~/.git-completion.bash ]; then
    source ~/.git-completion.bash;
fi

# run npm completion scrpt if it exists
if [ -f ~/.npm-completion.bash ]; then
    . ~/.npm-completion.bash;
fi

# run bash completion script if it exists
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

# run local bash configurations
if [ -f ~/.bash_local ]; then
    source ~/.bash_local;
fi
