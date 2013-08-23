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
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[cyan]%} = %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[purple]%} ? %{$reset_color%}"

# Default prompt
PROMPT='${blue}%n${reset_color}@${green}%m${red} %~$(git_prompt_info) ${blue}<${reset_color}%T${blue}>${reset_color}
$ '

# Right prompt - Git status
RPROMPT='$(git_prompt_status)%{$reset_color%}'

# ------------------------------------------------------------------------- }}}
# Aliases ----------------------------------------------------------------- {{{

# General {{{

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

# Basic directory operations
alias ...='cd ../..'
alias -- -='cd -'

# Super user
alias _='sudo'
alias ffs='sudo'

# Show history
alias history='fc -l 1'

# List direcory contents
alias lsa='ls -lah'
alias l='ls -la'
alias ll='ls -l'
alias la='ls -lA'

# }}}

# Bundler {{{

alias be="bundle exec"
alias bi="bundle install"
alias bl="bundle list"
alias bp="bundle package"
alias bo="bundle open"
alias bu="bundle update"

# }}}

# Git {{{

alias g='git'
compdef g=git
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
alias gd='git diff'
gdv() { git diff -w "$@" | view - }
compdef _git gdv=git-diff
alias gc='git commit -v'
compdef _git gc=git-commit
alias gc!='git commit -v --amend'
compdef _git gc!=git-commit
alias gca='git commit -v -a'
compdef _git gc=git-commit
alias gca!='git commit -v -a --amend'
compdef _git gca!=git-commit
alias gcmsg='git commit -m'
compdef _git gcmsg=git-commit
alias gco='git checkout'
compdef _git gco=git-checkout
alias gcm='git checkout master'
alias gr='git remote'
compdef _git gr=git-remote
alias grv='git remote -v'
compdef _git grv=git-remote
alias grmv='git remote rename'
compdef _git grmv=git-remote
alias grrm='git remote remove'
compdef _git grrm=git-remote
alias grset='git remote set-url'
compdef _git grset=git-remote
alias grup='git remote update'
compdef _git grset=git-remote
alias grbi='git rebase -i'
compdef _git grbi=git-rebase
alias grbc='git rebase --continue'
compdef _git grbc=git-rebase
alias grba='git rebase --abort'
compdef _git grba=git-rebase
alias gb='git branch'
compdef _git gb=git-branch
alias gba='git branch -a'
compdef _git gba=git-branch
alias gcount='git shortlog -sn'
compdef gcount=git
alias gcl='git config --list'
alias gcp='git cherry-pick'
compdef _git gcp=git-cherry-pick
alias glg='git log --stat --max-count=5'
compdef _git glg=git-log
alias glgg='git log --graph --max-count=5'
compdef _git glgg=git-log
alias gls='git log --graph --all --format=format:"%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %s%C(reset) %C(bold)- %an%C(reset)%C(bold yellow)%d%C(reset)" --abbrev-commit --date=relative'
compdef _git gls=git-log
alias gls2='git log --graph --all --format=format:"%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %s%C(reset) %C(bold)- %an%C(reset)" --abbrev-commit'
compdef _git gls2=git-log
alias glgga='git log --graph --decorate --all'
compdef _git glgga=git-log
alias glo='git log --oneline'
compdef _git glo=git-log
alias gss='git status -s'
compdef _git gss=git-status
alias ga='git add'
compdef _git ga=git-add
alias gm='git merge'
compdef _git gm=git-merge
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'
alias gclean='git reset --hard && git clean -dfx'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'
alias gf='git ls-files | grep'
alias gpoat='git push origin --all && git push origin --tags'
alias gmt='git mergetool --no-prompt'
compdef _git gm=git-mergetool

alias gg='git gui citool'
alias gga='git gui citool --amend'
alias gk='gitk --all --branches'
alias gsts='git stash show --text'

# Will cd into the top of the current repository
# or submodule.
alias grt='cd $(git rev-parse --show-toplevel || echo ".")'

# Git and svn mix
alias git-svn-dcommit-push='git svn dcommit && git push github master:svntrunk'
compdef git-svn-dcommit-push=git

alias gsr='git svn rebase'
alias gsd='git svn dcommit'
#
# Will return the current branch name
# Usage example: git pull origin $(current_branch)
#
function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function current_repository() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo $(git remote -v | cut -d':' -f 2)
}

# these aliases take advantage of the previous function
alias ggpull='git pull origin $(current_branch)'
compdef ggpull=git
alias ggpur='git pull --rebase origin $(current_branch)'
compdef ggpur=git
alias ggpush='git push origin $(current_branch)'
compdef ggpush=git
alias ggpnp='git pull origin $(current_branch) && git push origin $(current_branch)'
compdef ggpnp=git

# Pretty log messages
function _git_log_prettily(){
  if ! [ -z $1 ]; then
    git log --pretty=$1
  fi
}
alias glp="_git_log_prettily"
compdef _git glp=git-log

# }}}

# Laravel {{{

alias artisan='php artisan'
alias bob='php artisan bob::build'

# }}}

# ------------------------------------------------------------------------- }}}
