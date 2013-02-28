# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="rosstimson"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(bundler extract gem git git-flow github history-substring-search knife rails3 vagrant)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

###  My stuff:

# gpg-agent
GPG_TTY=$(tty)
export GPG_TTY

# Default mail sending to msmtp - needed for send-pr to use msmtp too.
export GNATS_ADDR=FreeBSD-gnats-submit@freebsd.org
export MAIL_AGENT="${MAIL_AGENT:-/usr/local/bin/msmtp -f ${USER}/.msmtprc -a rosstimson ${GNATS_ADDR}}"

# Use chruby for managing Ruby versions
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
# Setting a default Ruby
chruby 2.0.0-p0

# History
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
REPORTTIME=10 # print elapsed time when more than 10 seconds

setopt append_history
setopt share_history
setopt extended_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_no_store

# Set an alias for 'ls'
case `uname` in
  FreeBSD)
    alias ls='gnuls --color=auto -GF'
  ;;
  NetBSD)
    alias ls='ls -F'
  ;;
  OpenBSD)
    alias ls='colorls -GF'
  ;;
  Linux)
    alias ls='ls --color=always -F'
  ;;
esac

# Setting Term
#export TERM=rxvt-unicode

# Recognise custom termcap
if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

# Set EDITOR, PAGER etc.
export EDITOR='vim -f'
export PAGER='less -FSRX'

# Less Colors for Man Pages
# http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;33;246m'   # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# Other misc settings
LISTMAX=0
