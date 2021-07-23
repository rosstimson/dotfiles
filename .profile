# ~/.profile
#
# Shared config for the Bourne shell sh(1)
# and Bourne compatible shells bash(1), ksh(1), ash(1).

# Env vars
# --------------------------------------------------------------------
ALTERNATE_EDITOR='mg'
EDITOR='emacsclient -t'
GOPATH="${HOME}/code/go"
GPG_TTY="$(tty)"
KEYID='0x667A3481E4BB34F3'
LANG='en_GB.UTF-8'
LC_ALL='en_GB.UTF-8'
PAGER='less -i'
PATH="$HOME/.local/bin:$GOPATH/bin:$HOME/.cargo/bin:$HOME/.npm-global/bin:$HOME/bin:$PATH"
TZ='Europe/London'
VISUAL='emacsclient -c'

export ALTERNATE_EDITOR EDITOR GOPATH GPG_TTY KEYID LANG LC_ALL PAGER PATH PULSE_SERVER TZ VISUAL

# Source common aliases and functions
# --------------------------------------------------------------------
# shellcheck source=/home/rosstimson/.aliases
# Aliases first as functions might make use of them, e.g. sudo vs doas.
. "$HOME"/.aliases

# shellcheck source=/home/rosstimson/.functions
. "$HOME"/.functions


# Colours
# --------------------------------------------------------------------

# LS colours, made with http://geoff.greer.fm/lscolors/
# BSD
LSCOLORS="exfxcxdxbxxeadabagacaf"

# Linux
LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=0;44:cd=30;43:su=30;41:sg=30;46:tw=30;42:ow=30;45"

# exa colours (A modern replacement for ls written in Rust:  https://the.exa.website)
# Turn off some of the colours as this many colours is just obnoxious.
EXA_COLORS="uu=0:gu=0:ur=0:uw=0:ux=0:ue=0:gr=0:gw=0:gx=0:tr=0:tw=0:tx=0"

export LSCOLORS LS_COLORS EXA_COLORS


# SSH Agent
# --------------------------------------------------------------------

SSH_ENV="$HOME/.ssh/env"

start_ssh_agent() {
	echo 'Initialising new SSH agent...'
	# Intentionally don't set full path to ssh-agent, let $PATH
	# precedence take care of this.  This is especially important on
	# macOS where it is sometimes preferable to install a newer
	# version of OpenSSH via Homebrew, the path this gets installed
	# varies depending on chip architecture so hardcoding would add
	# complexity as you'd need some logic to set the path to the
	# binary first.
	ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
	echo 'Succeeded'
	chmod 600 "${SSH_ENV}"
	. "${SSH_ENV}" > /dev/null
}

# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
	. "${SSH_ENV}" > /dev/null
	ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
		start_ssh_agent;
	}
else
	start_ssh_agent;
fi


# Tools - Custom options for tools
# --------------------------------------------------------------------

# Skim - Fuzzy finder written in Rust: https://github.com/lotabout/skim
SKIM_DEFAULT_OPTIONS='--layout=reverse --color=light,fg:8,bg:15,current_bg:7,matched_bg:10,current_match:8'
export SKIM_DEFAULT_OPTIONS
