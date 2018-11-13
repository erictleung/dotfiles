# set vim bindings for CLI
set -o vi

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

# Make bash append rather than overwrite the history on disk
shopt -s histappend

# ls after cd
# source: https://dev.to/wulfmann/comment/6mp9
function cd {
    builtin cd "$@" && ls -Gltr --color
}

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
