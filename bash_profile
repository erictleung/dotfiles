# make ls work like I want it
alias ls="ls -Gltr"

##
# Your previous /Users/ericleung/.bash_profile file was backed up as /Users/ericleung/.bash_profile.macports-saved_2015-03-29_at_00:10:37
##

# MacPorts Installer addition on 2015-03-29_at_00:10:37: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.


# added by Anaconda 2.1.0 installer
export PATH="//anaconda/bin:$PATH"

# added manually May 13th, 2015
export PATH="/Applications/Postgres.app/Contents/Versions/9.4/bin:$PATH"

# added manual May 13th, 2015
export PATH="/Applications/neo4j/bin:$PATH"

# change limits for riak to work
ulimit -n 65536
ulimit -u 2048
export PATH=/usr/local/bin:$PATH

# add scala path manually Aug 12, 2015
export SCALA_HOME="/Applications/scala-2.11.7"
export PATH="$PATH:$SCALA_HOME/bin"
