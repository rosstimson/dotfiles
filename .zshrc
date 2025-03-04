# ~/.zshrc
#
# .zshenv -> .zprofile -> .zshrc
#
# .zshenv
# Read every time.  Be careful not to modify default behaviour of
# standard commands here such as setting aliases.
#
# .zprofile
# Read at login.
#
# .zshrc
# Read when interactive.


# Helper
# -----------------------------------------------------------------------------

# Set correct path if on an Apple Silicon Mac, Homebrew uses
# /opt/homebrew when on the arm64 (Apple Silicon) architecture whereas
# any other time it'd be /usr/local.
if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ] ; then
		homebrew_path='/opt/homebrew'
else
		homebrew_path='/usr/local'
fi


# Setup
# -----------------------------------------------------------------------------

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


# Source common aliases and functions
# --------------------------------------------------------------------
# shellcheck source=/home/rosstimson/.aliases
# Aliases first as functions might make use of them, e.g. sudo vs doas.
. "$HOME"/.aliases

# shellcheck source=/home/rosstimson/.functions
. "$HOME"/.functions


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


# History
# --------------------------------------------------------------------

HISTFILE=$HOME/.zsh_history
HISTSIZE=5000
SAVEHIST=5000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
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


# Colours
# --------------------------------------------------------------------

# LS colours, made with http://geoff.greer.fm/lscolors/
# BSD
export LSCOLORS="exfxcxdxbxxeadabagacaf"

# Linux
export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=0;44:cd=30;43:su=30;41:sg=30;46:tw=30;42:ow=30;45"

# exa colours (A modern replacement for ls written in Rust:  https://the.exa.website)
# Turn off some of the colours as this many colours is just obnoxious.
export EZA_COLORS="uu=0:gu=0:ur=0:uw=0:ux=0:ue=0:gr=0:gw=0:gx=0:tr=0:tw=0:tx=0"

# Skim - Fuzzy finder written in Rust: https://github.com/lotabout/skim
export SKIM_DEFAULT_OPTIONS='--layout=reverse --color=light,fg:8,bg:15,current_bg:7,matched_bg:10,current_match:8'

# https://github.com/zsh-users/zsh-autosuggestions
# Set the colour for the autosuggestion completions.  The tool/lib itself has
# been installed simply by its inclusion in ~/.zsh/lib
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#878787'

# kubectx - https://github.com/ahmetb/kubectx#customizing-colors
export KUBECTX_CURRENT_FGCOLOR=$(tput setaf 6) # blue text
export KUBECTX_CURRENT_BGCOLOR=$(tput setab 7) # white background


# SSH Agent
# --------------------------------------------------------------------

# SSH_ENV="$HOME/.ssh/env"

# start_ssh_agent() {
# 	echo 'Initialising new SSH agent...'
# 	# Intentionally don't set full path to ssh-agent, let $PATH
# 	# precedence take care of this.  This is especially important on
# 	# macOS where it is sometimes preferable to install a newer
# 	# version of OpenSSH via Homebrew, the path this gets installed
# 	# varies depending on chip architecture so hardcoding would add
# 	# complexity as you'd need some logic to set the path to the
# 	# binary first.
# 	ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
# 	echo 'Succeeded'
# 	chmod 600 "${SSH_ENV}"
# 	. "${SSH_ENV}" > /dev/null
# }

# # Source SSH settings, if applicable
# if [ -f "${SSH_ENV}" ]; then
# 	. "${SSH_ENV}" > /dev/null
# 	ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
# 		start_ssh_agent;
# 	}
# else
# 	start_ssh_agent;
# fi


# Tools settings
# --------------------------------------------------------------------

# Zoxide (https://github.com/ajeetdsouza/zoxide)
# Jump quickly to commonly used directories
eval "$(zoxide init zsh)"


# Prompt
# --------------------------------------------------------------------

# Starship (https://starship.rs)
eval "$(starship init zsh)"


# Nix (https://nixos.org/)
# --------------------------------------------------------------------

# Ensure Nix stuff is in $PATH.  I use the
# https://determinate.systems/posts/determinate-nix-installer but it
# puts stuff in /etc/bashrc which gets clobbered on macOS updates so
# ensure stuff gets sourced here.  There is a good overview of the
# problem and further potential fixes here, for now though this is a
# quick fix: https://checkoway.net/musings/nix/

[[ ! $(command -v nix) && -e "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]] && source "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"


# pnpm (https://pnpm.io/)
# --------------------------------------------------------------------

export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac


# direnv (https://direnv.net/) - unclutter your .profile
#
# direnv is an extension for your shell. It augments existing shells
# with a new feature that can load and unload environment variables
# depending on the current directory.
# --------------------------------------------------------------------
eval "$(direnv hook zsh)"


# Work
# -------------------------------------------------------------------

WORK_CONFIG=~/.zshrc-work && test -f $WORK_CONFIG && source $WORK_CONFIG
