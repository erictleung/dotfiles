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
