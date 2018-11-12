#!/bin/ksh

set -o emacs

# Aliases
#------------------------------------------------------------------------------

# CTRL-l to clear.
bind -m '^L'=clear'^J'

alias g="git"
alias tf="terraform"


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
	echo "(${branch})"
}

PS1='\h:\W$(git_prompt) $ '
