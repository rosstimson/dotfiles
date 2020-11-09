# ~/.profile
#
# Shared config for the Bourne shell sh(1)
# and Bourne compatible shells bash(1), ksh(1), ash(1).

# Env vars
# --------------------------------------------------------------------
ALTERNATE_EDITOR='mg'
EDITOR='emacsclient -t'
GOPATH="$HOME/code/go"
GPG_TTY="$(tty)"
KEYID='0x667A3481E4BB34F3'
LANG='en_GB.UTF-8'
LC_ALL='en_GB.UTF-8'
PAGER='less -i'
PATH="$HOME/.local/bin:$GOPATH/bin:$HOME/.cargo/bin:$HOME/.npm-global/bin:$HOME/bin:$PATH"
TZ='Europe/London'
VISUAL='emacsclient -c'

# Set Pulseaudo socket info so that flatpak apps can work with sound.
if command -v pulseaudio >/dev/null 2>&1; then
    PULSE_SERVER="unix:$HOME/.config/pulse/$(cat /var/lib/dbus/machine-id)-runtime/native"
fi

export GOPATH GPG_TTY KEYID LANG LC_ALL PAGER PATH PULSE_SERVER TZ ALTERNATE_EDITOR EDITOR VISUAL


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
	echo "Initialising new SSH agent..."
	/usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
	echo "Succeeded"
	chmod 600 "${SSH_ENV}"
	. "${SSH_ENV}" > /dev/null
}

# Source SSH settings, if applicable
if [ "$(uname -s)" = "Linux" ]; then
	if [ -f "${SSH_ENV}" ]; then
		. "${SSH_ENV}" > /dev/null
		pgrep -u "${USER}" ssh-agent | grep "${SSH_AGENT_PID}" > /dev/null || {
			start_ssh_agent;
		}
	else
		start_ssh_agent;
	fi
fi


# Tools - Custom options for tools
# --------------------------------------------------------------------

# Skim - Fuzzy finder written in Rust: https://github.com/lotabout/skim
SKIM_DEFAULT_OPTIONS='--layout=reverse --color=light,fg:8,bg:15,current_bg:7,matched_bg:10,current_match:8'
export SKIM_DEFAULT_OPTIONS

# sccache - Shared compilation cache: https://github.com/mozilla/sccache
# Set here rather than in Cargo config as you can't use $HOME in Cargo
# config and an absolute path means choosing between Linux/BSD style:
# '/home/rosstimson' or Mac style 'Users/rosstimson'.
RUSTC_WRAPPER="$HOME/.cargo/bin/sccache"
export RUSTC_WRAPPER

# Nix
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
	. "$HOME/.nix-profile/etc/profile.d/nix.sh"

	# When using Nix installed tools such as 'bat' some of them throw a warning:
	#
	#     $ bat /etc/apt/sources.list.d/pgdg.list
	#     /nix/store/2jysm3dfsgby5sw5jgj43qjrb5v79ms9-bash-4.4-p23/bin/bash: warning: setlocale: LC_ALL: cannot change locale (en_GB.UTF-8)
	#
	# By setting the location to the Nix locale stuff you can stop this harmless but annoying warning.
	if [ "$(uname -s)" = "Linux" ]; then
		LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
		export LOCALE_ARCHIVE_2_27
	fi
fi

# This is needed for nix-env installed dictionaries to get picked up by Hunspell
if [ -d "$HOME/.nix-profile/share/hunspell" ]; then
	DICPATH="$HOME/.nix-profile/share/hunspell"
	export DICPATH
fi

# Nix Home Manager (https://github.com/rycee/home-manager)
if [ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
	. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
fi
