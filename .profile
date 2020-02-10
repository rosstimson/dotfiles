#!/bin/sh

set -o emacs
set -o csh-history

HISTFILE="$HOME/.sh_history"
HISTSIZE="5000"

JAVA_HOME="/usr/local/jdk-11"
GOPATH="$HOME/code/go"

LANG="en_GB.UTF-8"
LC_ALL="en_GB.UTF-8"
KEYID="0x667A3481E4BB34F3"
GPG_TTY="$(tty)"
PAGER="less -i"
PATH="$JAVA_HOME/bin:$HOME/.local/bin:$GOPATH/bin:$HOME/.cargo/bin:$HOME/bin:/$PATH"

export LANG LC_ALL PATH KEYID GPG_TTY PAGER JAVA_HOME GOPATH

# Set Pulseaudo socket info so that flatpak apps can work with sound.
if command -v pulseaudio >/dev/null 2>&1; then
    export PULSE_SERVER="unix:$HOME/.config/pulse/$(cat /var/lib/dbus/machine-id)-runtime/native"
fi


# Colours
# -----------------------------------------------------------------------------

default="\033[0m"
green="\033[0;32m"


# Aliases
#------------------------------------------------------------------------------

# CTRL-l to clear.
bind -m '^L'=clear'^J'

alias _="doas"
alias ..="cd .."
alias ...="cd ../.."
alias -- --="cd -"

alias g="git"
alias h="fc -l 0"
alias l="ls -lah"
alias less="less -i"
alias t="tmux new-session -A -s rt"
alias tf="terraform"
alias pwup="cd $HOME/.password-store && git push -u origin master && cd -"
alias showpath="echo $PATH | tr -s ':' '\n'"


# Completions
#------------------------------------------------------------------------------

# Terraform
if [ -x /usr/local/bin/terraform ]; then
	set -A complete_terraform_1 -- apply console destroy env fmt get graph import init output plan providers push refresh show taint untaint validate version workspace
	set -A complete_tf_1 -- apply console destroy env fmt get graph import init output plan providers push refresh show taint untaint validate version workspace
fi


# Prompt
#------------------------------------------------------------------------------

if [ -e $HOME/bin/git-prompt-ksh.sh ]; then
	# shellcheck source=~/bin/git-prompt-ksh.sh
	. $HOME/bin/git-prompt-ksh.sh
	export GIT_PS1_SHOWDIRTYSTATE=true
	export GIT_PS1_SHOWUNTRACKEDFILES=true
	export GIT_PS1_SHOWUPSTREAM="auto"
fi

set -A big_gits \
	/usr/ports \
	/usr/src \
	/usr/xenocara

git_prompt() {
	# __git_ps1 causes some slowness on large repos, don't run this there.
	for d in "${big_gits[@]}"; do
		if echo "${PWD}" | grep -q "${d}"; then
			echo "(BIG)"
			return
		fi
	done
	branch=$(__git_ps1 "%s")
	if [ "${branch}" != "" ]; then
		echo "(${branch})"
	fi
}

PS1='${green}\h${default}:\W$(git_prompt) $ '
