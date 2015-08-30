# make ls work like I want it
alias ls="ls -Gltr"

# MacPorts Installer addition on 2015-03-29_at_00:10:37: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"

# added by Anaconda 2.1.0 installer
export PATH="//anaconda/bin:$PATH"

# added manually May 13th, 2015
export PATH="/Applications/Postgres.app/Contents/Versions/9.4/bin:$PATH"

# added manually May 13th, 2015
export PATH="/Applications/neo4j/bin:$PATH"

# change limits for riak to work
ulimit -n 65536
ulimit -u 2048
export PATH=/usr/local/bin:$PATH

# add scala path manually Aug 12, 2015
export SCALA_HOME="/Applications/scala-2.11.7"
export PATH="$PATH:$SCALA_HOME/bin"

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
