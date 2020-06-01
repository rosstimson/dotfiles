# ~/.zshrc
#
# Config for interactive zsh shells

# Source common settings such as env vars.
# shellcheck source=/home/rosstimson/.profile
. "$HOME"/.profile


# Setup --------------------------------------------------------------

# Path to search for autoloadable functions.
fpath=( $HOME/.zsh/lib "$fpath[@]" )
export FPATH
# Only unique entries please.
typeset -U fpath

# Sourcing lib files
for lib_file ($HOME/.zsh/lib/*.zsh); do
  . $lib_file
done

fpath=($HOME/.zsh/completions $fpath)


# Zsh settings
# --------------------------------------------------------------------

# Need these for autocompletion
autoload -U compinit
compinit

autoload bashcompinit
bashcompinit

# Enables the negation ^ operator for displaying files
setopt extendedglob

# When pattern matching fails, simply use the command as is
setopt no_nomatch

# Print elapsed time when more than 10 seconds
REPORTTIME=10

# Automatically decide when to page a list of completions
LISTMAX=0

# ls colours, also need this present for colours in the prompt
autoload colors; colors;  

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Colours
# --------------------------------------------------------------------

# 'less' colours for man pages
# These are not POSIX compliant so are not in ~/.profile
# https://github.com/koalaman/shellcheck/wiki/SC2039
LESS_TERMCAP_mb=$'\e[1;32m'
LESS_TERMCAP_md=$'\e[1;32m'
LESS_TERMCAP_me=$'\e[0m'
LESS_TERMCAP_se=$'\e[0m'
LESS_TERMCAP_so=$'\e[01;33m'
LESS_TERMCAP_ue=$'\e[0m'
LESS_TERMCAP_us=$'\e[1;4;31m'


# History
# --------------------------------------------------------------------

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


# Tools settings
# --------------------------------------------------------------------

# Jump quickly to frequently used directories.
# https://github.com/rupa/z/blob/master/z.sh
. $HOME/bin/z.sh


# Prompt
# --------------------------------------------------------------------

# Starship (https://starship.rs)
eval "$(starship init zsh)"


# For work
# --------------------------------------------------------------------
export VAULT_ADDR='https://vault.matillion-security.matillion.com:8200'
alias vl='vault login -method=oidc role=ops'
alias vpn='_ openvpn --config /etc/openvpn/client/matillion.conf'

eval $(vault-aws-creds.py -w)
