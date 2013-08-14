# Setup ------------------------------------------------------------------- {{{

# sourcing lib files
for lib_file ($HOME/.zsh/lib/*.zsh); do
  source $lib_file
done

# Sourcing the script for z completion (https://github.com/rupa/z)
. $HOME/.zsh/lib/z.sh

fpath=($HOME/.zsh/completions $fpath)

# ------------------------------------------------------------------------- }}}
# Custom ZSH -------------------------------------------------------------- {{{

# Need these for autocompletion
autoload -U compinit
compinit

# Basic settings
setopt no_beep
setopt autocd
setopt multios
setopt cdablevarS

# Enables the negation ^ operator for displaying files
setopt extendedglob

# Recognise custom termcap
if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

# Stop autocorrecting everything, list exempt commands in ~/.zsh_nocorrect
if [ -f ~/.zsh_nocorrect ]; then
  while read -r COMMAND; do
    alias $COMMAND="nocorrect $COMMAND"
  done < ~/.zsh_nocorrect
fi

# Set default editor and pager
export EDITOR='vim -f'
export PAGER='less -FSRX'

# Print elapsed time when more than 10 seconds
REPORTTIME=10

# Automatically decide when to page a list of completions
LISTMAX=0

# ls colors
autoload colors; colors;  # Also need this present for colours in the prompt
# LS colors, made with http://geoff.greer.fm/lscolors/
# BSD
export LSCOLORS="exfxcxdxbxegehabagacaf"
# Linux
export LS_COLORS='di=34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;47:su=0;41:sg=0;46:tw=0;42:ow=0;45:'

# ------------------------------------------------------------------------- }}}
# Less colours for man pages ---------------------------------------------- {{{

# http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;33;246m'   # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# ------------------------------------------------------------------------- }}}
# History ----------------------------------------------------------------- {{{

HISTFILE=$HOME/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

# ------------------------------------------------------------------------- }}}
# Env vars ---------------------------------------------------------------- {{{

# gpg-agent
GPG_TTY=$(tty)
export GPG_TTY

# Default mail sending to msmtp - needed for send-pr to use msmtp too.
export GNATS_ADDR=FreeBSD-gnats-submit@freebsd.org
export MAIL_AGENT="${MAIL_AGENT:-/usr/local/bin/msmtp -f ${USER}/.msmtprc -a rosstimson ${GNATS_ADDR}}"

# For use with hub
export GITHUB_USER='rosstimson'

# ------------------------------------------------------------------------- }}}
# Tools settings ---------------------------------------------------------- {{{

# Use chruby for managing Ruby versions
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
# Setting a default Ruby
chruby 2.0

# Python Virtualenv
export WORKON_HOME=$HOME/code/python
source /usr/local/bin/virtualenvwrapper_lazy.sh

# ------------------------------------------------------------------------- }}}
# Prompt ------------------------------------------------------------------ {{{

# Enable prompt customization
setopt prompt_subst

# Colour variables to make prompt theme more readable
export black=$'%{\e[0;30m%}'
export red=$'%{\e[0;31m%}'
export green=$'%{\e[0;32m%}'
export brown=$'%{\e[0;33m%}'
export blue=$'%{\e[0;34m%}'
export purple=$'%{\e[0;35m%}'
export cyan=$'%{\e[0;36m%}'
export light_gray=$'%{\e[0;37m%}'
export dark_gray=$'%{\e[1;30m%}'
export light_red=$'%{\e[1;31m%}'
export light_green=$'%{\e[1;32m%}'
export yellow=$'%{\e[1;33m%}'
export light_blue=$'%{\e[1;34m%}'
export pink=$'%{\e[1;35m%}'
export light_cyan=$'%{\e[1;36m%}'
export white=$'%{\e[1;37m%}'
export reset_color=$'%{\e[0m%}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[blue]%}[%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}]"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}]"

ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[green]%} + %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%} * %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%} x %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[blue]%} _ %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[pink]%} = %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[purple]%} ? %{$reset_color%}"

# Default prompt
PROMPT='${blue}%n${reset_color}@${green}%m${red} %~$(git_prompt_info) ${blue}<${reset_color}%T${blue}>${reset_color}
$ '

# Right prompt - Git status
RPROMPT='$(git_prompt_status)%{$reset_color%}'

# ------------------------------------------------------------------------- }}}

# Aliases ----------------------------------------------------------------- {{{


# Set an alias for 'ls' so colours can be used across OSs
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
  *)
    alias ls='ls -G'
  ;;
esac

# ------------------------------------------------------------------------- }}}
