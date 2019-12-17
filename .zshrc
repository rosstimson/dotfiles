########################################
#
# .zshrc: for interactive shells (zsh -i)
#
########################################

# Since .zshrc is only run once per interactive shell, most of the 
# configuring of the login session gets done here.  A few of .zshenv's
# settings are overridden here in the knowledge that the user is now
# able to give commands to the shell.


####################
# zsh-related things

###
# Shell Options
# Only those options relating to interactive shells.  The others were
# done in .zshenv already.
# Ones in capitals are variations from the default ZSH behaviour.
setopt \
  always_to_end \
  auto_menu \
  autocd \
  cdablevars \
  complete_in_word \
  no_beep

unsetopt \
  flowcontrol \
  menu_complete


# Setup ------------------------------------------------------------------- {{{

# Sourcing lib files
for lib_file ($HOME/.zsh/lib/*.zsh); do
  source $lib_file
done

fpath=($HOME/.zsh/completions $fpath)

# Helper function to test if command exists
_command_exists() {
  type "$1" &> /dev/null;
}


# ------------------------------------------------------------------------- }}}
# Custom ZSH -------------------------------------------------------------- {{{

# Need these for autocompletion
autoload -U compinit
compinit

# Enables the negation ^ operator for displaying files
setopt extendedglob

# When pattern matching fails, simply use the command as is
setopt no_nomatch

# Recognise custom termcap
if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

# Set default editor and pager
export EDITOR='mg'
export PAGER='less -FRi'

# Print elapsed time when more than 10 seconds
REPORTTIME=10

# Automatically decide when to page a list of completions
LISTMAX=0

# ls colors
autoload colors; colors;  # Also need this present for colours in the prompt
# LS colors, made with http://geoff.greer.fm/lscolors/
# BSD
export LSCOLORS="exfxcxdxbxxeadabagacaf"
# Linux
export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=0;44:cd=30;43:su=30;41:sg=30;46:tw=30;42:ow=30;45"

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# ------------------------------------------------------------------------- }}}
# Less colours for man pages ---------------------------------------------- {{{

export LESS_TERMCAP_mb=$(tput setaf 2) # green
export LESS_TERMCAP_md=$(tput setaf 6) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 0; tput setab 2) # black on green
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white underlined
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)
export GROFF_NO_SGR=1         # For Konsole and Gnome-terminal

# ------------------------------------------------------------------------- }}}
# History ----------------------------------------------------------------- {{{

HISTFILE=$HOME/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
HISTTIMEFORMAT="%d/%m/%y %T "

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

# Search through history with cursor keys (up/down) matching everything up
# to current cursor postition.  Corresponding key bindings are in
# ~/.zsh/completions/key-bindings.zsh
autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# ------------------------------------------------------------------------- }}}
# Env vars ---------------------------------------------------------------- {{{

# gpg-agent - Git gpg signing won't work without this.
export GPG_TTY=$(tty)

# Default mail sending to msmtp - needed for send-pr to use msmtp too.
export GNATS_ADDR=FreeBSD-gnats-submit@freebsd.org
export MAIL_AGENT="${MAIL_AGENT:-/usr/local/bin/msmtp -f ${USER}/.msmtprc -a rosstimson ${GNATS_ADDR}}"

# For use with hub
export GITHUB_USER='rosstimson'

# ------------------------------------------------------------------------- }}}
# Tools settings ---------------------------------------------------------- {{{

# Use chruby for managing Ruby versions
if [ -d /usr/local/share/chruby ]; then
  source /usr/local/share/chruby/chruby.sh
  source /usr/local/share/chruby/auto.sh
  # Setting a default Ruby
  chruby 2.4
fi

# pyenv shims and autocompletion
if _command_exists pyenv; then
  eval "$(pyenv init -)"
fi

# Use gh (Github CLI client / helper)
if _command_exists gh; then
  eval  "$(gh alias -s)"
fi

# Jump quickly to frequently used directories.
# https://github.com/rupa/z/blob/master/z.sh
. $HOME/bin/z.sh


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


PROMPT='${green}%m${reset_color}:%c$(pretty-git-prompt) %# '

# ------------------------------------------------------------------------- }}}
# Aliases ----------------------------------------------------------------- {{{

# General {{{

# Set an alias for 'ls' so colours can be used across OSs
case `uname` in
  FreeBSD)
    alias ls='ls -GF'
  ;;
  NetBSD)
    alias ls='ls -F'
  ;;
  OpenBSD)
    alias ls='gls --color=always -F'
  ;;
  Linux)
    alias ls='ls --color=always -F'
  ;;
  *)
    alias ls='ls -G'
  ;;
esac

# Basic directory operations
alias ...='cd ../..'
alias -- -='cd -'

# Super user
alias sudo='sudo '  # Note the trailing space, this allows sudo to use aliases.
alias _='sudo '
alias ffs='sudo '

# List direcory contents
alias l='ls -lah'
alias ll='ls -l'
alias lr='ls -lr'

# Tmux - Attach to previous session or start a new one named rt.
alias t='tmux new-session -A -s rt'

# Terminal based file manager: https://github.com/jarun/nnn
alias n='nnn'

# History
alias h='history -E 1'

# IP commands
alias ip='ip --color'
alias ipb='ip --color --brief'

# Git
alias g='git'

# Kubernetes
alias k='kubectl'

# Terraform
alias tf='terraform'

# Emacs
# Function needed instead of alias so params can be passed.
e() {emacsclient -a "" -c "$@" &}

# Pretty print $PATH
alias showpath="echo $PATH | tr -s ':' '\n'"

# }}}

# Docker {{{

alias d='podman'
alias da='podman attach'
alias db='podman build'
alias dc='podman commit'                    # Clobbers GNU dc calculator tool
alias dclean='podman rmi -f $(podman images -q -a -f dangling=true)' # Removes all untagged images
alias ddr='podman run -d'
alias de='podman exec -t -i'
alias di='podman images'
alias dl='podman pull'
alias dlg='podman logs'
alias dm='docker-machine'
alias dp='podman push'
alias dps='podman ps'
alias dr='podman run'
alias dri='podman run -it --rm'
alias drm='podman rm'
alias drma='podman rm $(podman ps -a -q)'    # Remove all containers
alias drmi='podman rmi'
alias ds='podman stop'
alias dsa='podman stop $(podman ps -a -q)'  # Stop all containers
alias dt='podman tag'

# }}}

# Rust {{{

# Function needed instead of alias so params can be passed.
rrun() {rustc $1.rs && ./$1}

alias ru='rustup'

# }}}

# Python {{{

# Globally install pip packages, this is needed to overrid the
# `require-virtualenv = true` which is set in `pip.conf`. This lets you
# 'bypass' this restriction when necessary to install things like pipenv
# or jedi; just use `gpip` instead of the usual `pip`.
gpip() {
  PIP_REQUIRE_VIRTUALENV="" pip "$@"
}

# ------------------------------------------------------------------------- }}}

# AWS CLI Completion
if [ -f /usr/bin/aws_zsh_completer.sh ]; then
  source /usr/bin/aws_zsh_completer.sh
fi

# Kubernetes CLI Completion
if _command_exists kubectl; then
  source <(kubectl completion zsh)
fi
