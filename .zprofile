# ~/.zshenv
#
# .zshenv -> .zprofile -> .zshrc
#
# .zhenv
# Read every time.  Be careful not to modify default behaviour of
# standard commands here such as setting aliases.
#
# .zprofile
# Read at login.
#
# .zshrc
# Read when interactive.


# Env vars
# --------------------------------------------------------------------

export ALTERNATE_EDITOR='mg'
export EDITOR='emacsclient -t'
export GOPATH="${HOME}/code/go"
export GPG_TTY="$(tty)"
export HOMEBREW_NO_ASK=1 # Homebrew 6 prompts before install/upgrade by default.
export KEYID='0x667A3481E4BB34F3'
export LANG='en_GB.UTF-8'
export LC_ALL='en_GB.UTF-8'
export PAGER='less -i'
export TZ='Europe/London'
export VISUAL='emacsclient -c'
export XDG_CONFIG_HOME="${HOME}/.config"

# For compiling stuff on Apple Silicon Macs.
export CPATH=/opt/zerobrew/include:/opt/homebrew/include
export LIBRARY_PATH=/opt/zerobrew/lib:/opt/homebrew/lib


# PATH
# -------------------------------------------------------------------

# (N-/): do not register if the directory does not exists
# (Nn[-1]-/)
#
#  N   : NULL_GLOB option (ignore path if the path does not match the glob)
#  n   : Sort the output
#  [-1]: Select the last item in the array
#  -   : follow the symbol links
#  /   : ignore files
#  t   : tail of the path

path=($HOME/.local/share/mise/shims(N-/)
	  $HOME/.local/bin(N-/)
	  $GOPATH/bin(N-/)
	  $HOME/.cargo/bin(N-/)
	  $HOME/.npm-global/bin(N-/)
	  $HOME/Library/pnpm/bin(N-/)
	  $HOME/.krew/bin(N-/)
	  $HOME/bin(N-/)
	  /usr/local/MacGPG2/bin(N-/)
	  $HOME/.zerobrew/bin(N-/)
	  /opt/zerobrew/bin(N-/)
	  /opt/zerobrew/prefix/bin(N-/)
	  /opt/homebrew/{bin,sbin}(N-/)
	  /Applications/Postgres.app/Contents/Versions/latest/bin(N-/)
	  /Library/TeX/texbin/(N-/)
	  /usr/local/{bin,sbin}
	  /usr/{bin,sbin}
	  /{bin,sbin}
	 )

# Remove any duplicates in $PATH and export.
typeset -gU path
export PATH
