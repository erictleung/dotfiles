# add `~/bin` to path
export PATH="$HOME/bin:$PATH"

# enable tab completion
source ~/.git-completion.bash

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

# run npm completion scrpt if it exists
if [ -f ~/.npm-completion.bash ]; then
    . ~/.npm-completion.bash;
fi
