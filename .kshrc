# ~/.kshrc

set -o emacs
set -o csh-history

HISTFILE="$HOME/.sh_history"
HISTSIZE="5000"

# CTRL-l to clear.
bind -m '^L'=clear'^J'


# Colours
# -----------------------------------------------------------------------------

default="\033[0m"
green="\033[0;32m"


# Completions
#------------------------------------------------------------------------------

# Terraform
if [ -x /usr/local/bin/terraform ]; then
	set -A complete_terraform_1 -- apply console destroy env fmt get graph import init output plan providers push refresh show taint untaint validate version workspace
	set -A complete_tf_1 -- apply console destroy env fmt get graph import init output plan providers push refresh show taint untaint validate version workspace
fi


# Prompt
#------------------------------------------------------------------------------

if [ -e "$HOME"/bin/git-prompt-ksh.sh ]; then
	# shellcheck source=/home/rosstimson/bin/git-prompt-ksh.sh
	. "$HOME"/bin/git-prompt-ksh.sh
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
	# shellcheck disable=SC2154
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
