#####################################################################
#           mates zshrc file v0.3, based on:
#               mako's zshrc file, v0.1
#
#
######################################################################

#{{{ Options
# emacs key bindings
set -o emacs
# why would you type 'cd dir' if you could just type 'dir'?
setopt AUTO_CD
# Spell check commands!  (Sometimes annoying)
setopt CORRECT
# This makes cd=pushd
setopt AUTO_PUSHD
# blank pushd goes to home
setopt PUSHD_TO_HOME
# this will ignore multiple directories for the stack.  Useful?  I dunno.
setopt PUSHD_IGNORE_DUPS
# 10 second wait if you do something that will delete everything.  I wish I'd had this before...
setopt RM_STAR_WAIT
# beeps are annoying
setopt NO_BEEP
# use magic (this is default, but it can't hurt!)
setopt ZLE

# next lets set some enviromental/shell pref stuff up
setopt INC_APPEND_HISTORY SHARE_HISTORY
# Don't overwrite, append!
setopt APPEND_HISTORY
# do NOT nice bg commands
unsetopt BG_NICE
# Save the time and how long a command ran
setopt EXTENDED_HISTORY
setopt MENUCOMPLETE
setopt ALL_EXPORT
# If a line starts with a space, don't save it.
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE
# Pretty    Obvious.  Right?
setopt HIST_REDUCE_BLANKS
# Even if there are commands inbetween commands that are the same, still only save the last one
setopt HIST_IGNORE_ALL_DUPS
# If I type cd and then cd again, only save the last one
setopt HIST_IGNORE_DUPS

setopt prompt_subst

# Set/unset  shell options
setopt   notify correct globdots pushdtohome cdablevars autolist
setopt   autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash correctall


#}}}

#{{{ ZSH Modules

autoload -U compinit promptinit zcalc zsh-mime-setup
compinit
promptinit
zsh-mime-setup

# Autoload zsh modules when they are referenced
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
#zmodload -ap zsh/mapfile mapfile
#}}}

#{{{ Variables
PATH="/usr/lib/ccache/bin:$HOME/.rbenv/bin:/usr/local/sbin/:/bin:/sbin:/usr/bin:/usr/sbin:/home/mates/.cabal/bin:/home/mates/bin:/opt/android-sdk/tools:/opt/android-sdk/platform-tools:/usr/share/java/apache-ant/bin:/home/mates/.npm/bin:/home/mates/.gem/ruby/2.3.0/bin:/home/mates/.gem/ruby/2.4.0/bin/:/home/mates/.local/bin:/home/mates/bin:/home/mates/dev/igalia/depot_tools:$PATH"

TZ="Europe/Berlin"
# TZ="Brazil/East"
# TZ="EDT"
MYVIMRC="$HOME/.vimrc"
GOPATH="$HOME/gocode/3rdparty:$HOME/gocode/own"
HISTFILE=$HOME/.zhistory
HISTSIZE=10000
SAVEHIST=10000
HOSTNAME="`hostname`"
PAGER='less'
EDITOR='nvim -u ~/.vimrc'
TERMINAL='uxterm'
LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
QT_GRAPHICSSYSTEM=native
JAVA8_HOME=/usr/lib/jvm/java-8-openjdk/bin/java
MYVIMRC=~/.vimrc

autoload -Uz vcs_info
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
   colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
   eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
   eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
   (( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

precmd() {
  psvar=()
  vcs_info
  [[ -n $vcs_info_msg_0_ ]] && psvar[1]="$vcs_info_msg_0_"
}
PR_RESET="%{${reset_color}%}";

PS1="[$PR_BLUE%n$PR_WHITE@$PR_GREEN%U%m%u$PR_NO_COLOR:$PR_RED%2c$PR_NO_COLOR]%(1v.%F$PR_BLUE%1v%f.)%(!.#.$) "
RPS1="$PR_LIGHT_YELLOW(%D{%m-%d %H:%M})$PR_NO_COLOR"


#LANGUAGE=
LC_ALL='en_US.UTF-8'
LANG='en_US.UTF-8'
LC_CTYPE=C

# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='rg -l .'
export XDG_CONFIG_HOME=~/.config

unsetopt ALL_EXPORT

# Named Directories
# paps=/home/mates/home_repo/school/grad/papers/

#}}}

#{{{ Functions
# automatically display the current directory in the xterm title bar
chpwd() {
  [[ -t 1 ]] || return
  case $TERM in
    sun-cmd) print -Pn "\e]l%~\e\\"
      ;;
    *xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;%~\a"
      ;;
  esac
}

# show colorized svn diff
function svndiff () { svn diff $@ | colordiff | less -R; }
function gitdiff () { git diff $@ | colordiff | less -R; }

#Quickly tar a dif or file
function qtar {
  tar czf ./$1.tgz $1
}

extract () {
   if [ -f $1 ] ; then
       case $1 in
              *.tar.bz2)   tar xjf $1      ;;
              *.tar.gz)    tar xzf $1      ;;
              *.bz2)       bunzip2 $1      ;;
              *.rar)       unrar x $1      ;;
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

# zip the contents a directory as a ccz using the name of the current ccz in the directory
pccz () {
  ccz=`ls *.ccz | sed -n 1p`
  rm -i *.ccz
  zip -r $ccz *
}

# move a ccz file to a temp directory and unzip it there
occz () {
   if [ -f $1 ] ; then
     dir=`mktemp -d`
     echo $dir
     mv $1 $dir
     cd $dir
     unzip *.ccz
   else
       echo "'$1' is not a valid file"
   fi
}

# Another method for quick change directories. Add this to your ~/.zshrc, then just enter “cd ..../dir”
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot

eval `keychain --eval id_ed25519`

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | sed "s/.* //")
}

#}}}

#{{{ Aliases

# Query wikipedia
wiki() { dig +short txt $1.wp.dg.cx;}

alias ode='ssh odenot.hopto.org -p 6348'

# display top ten running processes sorted by memory usage
alias tps='ps aux | sort -nk +4 | tail'
alias sabesp='~/.local/bin/sabesp'
alias gm=/home/mates/dev/igalia/chromium/src/v8/tools/dev/gm.py
alias v8gen=/home/mates/dev/igalia/chromium/src/v8/tools/dev/v8gen.py

alias tmux='tmux -2'

alias v='nvim -u ~/.vimrc'
alias install='yaourt -S '
alias search='yaourt -Ss'
alias update='yaourt -Syua'
alias cp='nocorrect cp -r '
alias scp='scp -r '
alias mgrep='grep -A1 -B1 -n --color -r --exclude="*.{swp,pyc}"'
alias hgrep='history 1 | ag '
alias psgrep="ps aux | ag "
alias mget='wget --random-wait -r -p -k -e robots=off -U Mozilla -t45 -l0 '
alias reload="source ~/.zshrc"
alias rcedit='nvim ~/.zshrc'
alias nvim='nvim -u ~/.vimrc'
alias psme='ps aux | grep `whoami`'
alias findf='find . -type f -name'

alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'

# du (disk usage) hunman readable, summary, one file system, total
alias du="du -hsc "

# df (mounts) human readable
alias df="df -h "

alias man='LC_ALL=C LANG=C man'
alias f=finger
alias ll='ls -al'
alias ls='ls --color=auto '
alias =clear

# git stuffs, weeeee
alias lg="git log --graph --pretty=format:'%Cred%h%Creset %Cgreen%t%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias lgn="git log --name-only"
alias gp="git pull"
alias gst="git status ."
alias gd="git diff ."
alias gdm="git diff main"
alias gmm="git merge main"
alias gdc="git diff --cached ."
alias gpu="git push origin"
alias grp="git push -u origin HEAD"
alias gcb="git co -b "
alias gco="git co "
alias ga="git add -u"
alias gc="git commit"
alias gca="git commit --amend"
alias gcm="git checkout main"
alias lb="git for-each-ref --sort=-committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'"

v8=~/dev/igalia/chromium/src/v8
chrom=~/dev/igalia/chromium/src/
alias re="lein repl"
#}}}

#{{{ Key bindings
bindkey '\e.' insert-last-word
bindkey '®' insert-last-word
bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char
bindkey ";5D" backward-word
bindkey ";5C" forward-word
bindkey '^[OH' beginning-of-line
bindkey '^[OF' end-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[6~' down-line-or-history
#bindkey "^r" history-incremental-search-backward
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
#}}}

#{{{ Completion Stuff
zstyle ':completion:*' use-cache on
zstyle ':completion:*' hosts off
zstyle ':completion:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' accept-exact '*(N)'

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
#zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
# on processes completion complete all user processes
# zstyle ':completion:*:processes' command 'ps -au$USER'

## add colors to processes for kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:processes-names' command 'ps axho command'
#
# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.log' '*?.aux' '*?.pyc' '*?.class' '*?.o' '*?.c~' \
    '*?.old' '*?.pro' '*?.hi'

# ignore completion functions (until the _ignored completer)
# Ignore completion functions for commands you don't have
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm apache bin daemon games gdm halt ident junkbust lp mail \
        mailnull named news nfsnobody nobody nscd ntp operator pcap \
        postgres radvd rpc rpcuser rpm shutdown squid sshd sync uucp vcsa \
        xfs avahi-autoipd avahi backup messagebus beagleindex debian-tor \
        dhcp dnsmasq fetchmail firebird gnats haldaemon hplip irc klog \
        list man cupsys postfix proxy syslog www-data mldonkey sys snort \
        mkdir

# SSH Completion
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

# cd will never select the parent directory (e.g.: cd ../<TAB>)
zstyle ':completion:*:cd:*' ignore-parents parent pwd
#}}}
#

export CCACHE_SLOPPINESS=include_file_mtime
export CCACHE_CPP2=yes
export CCACHE_BASEDIR=/home/mates/dev/igalia/chromium
PATH=/home/mates/dev/igalia/chromium/src/third_party/llvm-build/Release+Asserts/bin:$PATH
PYTHONPATH=/home/mates/dev/igalia/chromium/src/build:$PYTHONPATH


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source /etc/profile.d/autojump.zsh
