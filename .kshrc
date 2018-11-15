#!/bin/ksh

set -o emacs
set -o csh-history

HISTFILE="~/.ksh_history"
HISTSIZE=2000

export LANG=en_GB.UTF-8

export KEYID=0x667A3481E4BB34F3
export GPG_TTY=$(tty)


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
alias less="less -Fi"
alias tf="terraform"


# Completions
#------------------------------------------------------------------------------

if [ -e /usr/local/bin/terraform ]; then
	set -A complete_terraform_1 -- apply console destroy env fmt get graph import init output plan providers push refresh show taint untaint validate version workspace
	set -A complete_tf_1 -- apply console destroy env fmt get graph import init output plan providers push refresh show taint untaint validate version workspace
fi


# Prompt
#------------------------------------------------------------------------------

if [ -e ~/.git-prompt ]; then
	# shellcheck source=~/.git-prompt
	. ~/.git-prompt
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