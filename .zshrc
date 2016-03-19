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

# Sourcing script for Git prompt
. $HOME/.zsh/lib/git-prompt/zshrc.sh

# Sourcing the script for z completion (https://github.com/rupa/z)
. $HOME/.zsh/lib/z.sh

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

# Stop autocorrecting everything, list exempt commands in ~/.zsh_nocorrect
if [ -f ~/.zsh_nocorrect ]; then while read -r COMMAND; do
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

# Set pretty colourscheme
# Base16 Shell (https://github.com/chriskempson/base16-shell)
BASE16_SHELL="$HOME/.base16-shell/base16-atelierlakeside.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

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
if [ -d /usr/local/share/chruby ]; then
  source /usr/local/share/chruby/chruby.sh
  source /usr/local/share/chruby/auto.sh
  # Setting a default Ruby
  chruby 2.3
fi

# pyenv shims and autocompletion
if _command_exists pyenv; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# Use gh (Github CLI client / helper)
if _command_exists gh; then
  eval  "$(gh alias -s)"
fi


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


PROMPT='${green}%m${reset_color}:%c$(git_super_status) %# '

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
    alias ls='colorls -GF'
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

# Show history
alias history='fc -l 1'

# List direcory contents
alias l='ls -lah'
alias ll='ls -l'
alias lr='ls -lr'

# }}}

# Bundler {{{

alias be='bundle exec '
alias bi='bundle install'
alias bl='bundle list'
alias bp='bundle package'
alias bo='bundle open'
alias bu='bundle update'

# }}}

# Git {{{

alias g='git'
compdef g=git
alias gi='git init'
compdef _git gi=git-init
alias gst='git status'
compdef _git gst=git-status
alias gd='git diff'
compdef _git gd=git-diff
alias gl='git pull'
compdef _git gl=git-pull
alias gup='git pull --rebase'
compdef _git gup=git-fetch
alias gp='git push'
compdef _git gp=git-push
alias gc='git commit -v'
compdef _git gc=git-commit
alias gco='git checkout'
compdef _git gco=git-checkout
alias gr='git remote'
compdef _git gr=git-remote
alias grbi='git rebase -i'
compdef _git grbi=git-rebase
alias grbc='git rebase --continue'
compdef _git grbc=git-rebase
alias grba='git rebase --abort'
compdef _git grba=git-rebase
alias gbr='git branch'
compdef _git gbr=git-branch
alias gcount='git shortlog -sn'
compdef gcount=git
alias gcp='git cherry-pick'
compdef _git gcp=git-cherry-pick
alias glg='git log --stat --max-count=5'
compdef _git glg=git-log
alias glgg='git log --graph --max-count=5'
compdef _git glgg=git-log
alias gls='git log --graph --all --format=format:"%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %s%C(reset) %C(bold)- %an%C(reset)%C(bold yellow)%d%C(reset)" --abbrev-commit --date=relative'
compdef _git gls=git-log
alias glo='git log --oneline'
compdef _git glo=git-log
alias ga='git add'
compdef _git ga=git-add
alias gm='git merge'
compdef _git gm=git-merge
alias gmt='git mergetool --no-prompt'
compdef _git gmt=git-mergetool

# Will cd into the top of the current repository
# or submodule.
alias grt='cd $(git rev-parse --show-toplevel || echo ".")'

# }}}

# Docker {{{

alias d='docker'
alias da='docker attach'
alias db='docker build'
alias dc='docker commit'                    # Clobbers GNU dc calculator tool
alias de='docker exec -t -i'
alias di='docker images'
alias dl='docker pull'
alias dp='docker push'
alias dps='docker ps'
alias dr='docker run'
alias dri='docker run -it --rm'
alias ddr='docker run -d'
alias ds='docker stop'
alias drm='docker rm'
alias drmi='docker rmi'
alias drma='docker rm $(docker ps -a -q)'    # Remove all containers
alias dsa='docker stop $(docker ps -a -q)'  # Stop all containers
alias dclean='docker rmi -f $(docker images -q -a -f dangling=true)' # Removes all untagged images
alias dm='docker-machine'

# }}}

# Test-Kitchen {{{

alias k='kitchen'
alias kc='kitchen create'
alias kd='kitchen destroy'
alias kl='kitchen login'
alias kls='kitchen list'
alias ks='kitchen setup'
alias kt='kitchen test'
alias kv='kitchen verify'
alias kr='kitchen converge'

# }}}

# Python {{{

alias upeggs="yolk -U | awk {'print $1'} | xargs pip install -U"

# }}}

# ------------------------------------------------------------------------- }}}

# AWS CLI Completion
if [ -f /usr/bin/aws_zsh_completer.sh ]; then
  source /usr/bin/aws_zsh_completer.sh
fi
