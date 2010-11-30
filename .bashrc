export EDITOR='vi'

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Aliases for things I do a lot
# useful both for brevity (laziness) and cheao pseudo commands
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

alias cade='ssh mates@lab2-26.eng.utah.edu'

# web dev
alias ar='/etc/init.d/apache2 restart'
alias al='tail -n 50 -f /var/log/apache2/error_log'
alias vl='tail -n 50 -f /server/vistrails/git/vistrails/rpcserver.log'
alias wl='tail -n 50 -f /server/crowdlabs/logs/web_server.log'
alias yin='yast webpin_package_search'
alias spinax='source /server/packages/up-pinax-env/bin/activate'


# Some ls lazyness
alias d="ls -G"
alias ls="ls -G"
alias ll="ls -h -G -l"
alias lla="ls -h -G -Al"
alias la="ls -G -A"
alias lt="ls -h  -G -lt"
alias ltr="ls -h  -G -ltr"

# Only look for directories when using cd 
complete -d cd

# Recursive copy
alias cp='cp -r '

# Recursive secure copy
alias scp='scp -r '

# Always do X forwarding for ssh sessions
#alias ssh='ssh -X '

# du (disk usage) hunman readable, summary, one file system, total
alias du="du -hsc "

# df (mounts) human readable
alias df="df -h "

# A nice scheme that makes grep output more decipherable
alias mgrep='grep -A2 -B2 -n --color -r --exclude=*.{swp,pyc}'

# search my bash history
alias hgrep='history | grep '

# Quick and dirty web fetch
alias mget='wget -r -k -t45 -l2 -o log '

# Reloads the bashrc so all recent changes take effect
alias reload="source ~/.bashrc"

# remove all .tmp files in the current working dir
alias deltmp="rm *.tmp"

# Don't ask when arremin
#alias rm="rm -f "

# rm -Rf shortcut
#alias del="rm -Rf "

# Shortcut for operations to my .bashrc
alias rcedit='vi ~/.bashrc'
alias rccommit='svn commit ~/.bashrc'
alias rcup="svn update ~/.bashrc"

# job control 
alias k9="kill -9 "
alias ka9="killall -9 "
alias pgrep="ps aux|grep "

# Show all the processes owned by my user
alias psme='ps aux|grep `whoami`'

alias ipy=ipython

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Custom functions
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Aliases for fast ssh logins
#function quick_logins
#{
#	alias webserver='ssh matthew@192.168.1.10'
#	alias fileserver='ssh matthew@192.168.1.11'
#}

function formattedGitBranch {
    _branch="$(git branch 2>/dev/null | sed -e "/^\s/d" -e "s/^\*\s//")"
    test -n "$_branch" && echo -e " (git)-[$_branch]-"
}

# show colorized svn diff
function svndiff () { svn diff $@ | colordiff | less -R; }
function gitdiff () { git diff $@ | colordiff | less -R; }

#Quickly tar a dif or file
function qtar
{
  tar czf ./$1.tgz $1
}

#
extract () {
   if [ -f $1 ] ; then
       case $1 in
              *.tar.bz2)   tar xjf $1      ;;
              *.tar.gz)    tar xzf $1      ;;
              *.bz2)       bunzip2 $1      ;;
              *.rar)       rar x $1        ;;
              *.gz)        gunzip $1       ;;
              *.tar)       tar xf $1       ;;
              *.tbz2)      tar xjf $1      ;;
              *.tgz)       tar xzf $1      ;;
              *.zip)       unzip $1        ;;
              *.Z)         uncompress $1   ;;
              *)           echo "'$1' cannot be extracted via extract()" ;;
      esac
   else
       echo "'$1' is not a valid file"
   fi
}


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# misc
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi


# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
#case "$TERM" in
#xterm-color)
#    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#    ;;
#*)
#    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#    ;;
#esac

# Comment in the above and uncomment this below for a color prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(formattedGitBranch) $ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

export TERM=xterm-256color

GREP_OPTIONS="--exclude-dir=\.svn"
export GREP_OPTIONS

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi


# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
