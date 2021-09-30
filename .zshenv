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
export KEYID='0x667A3481E4BB34F3'
export LANG='en_GB.UTF-8'
export LC_ALL='en_GB.UTF-8'
export PAGER='less -i'
export TZ='Europe/London'
export VISUAL='emacsclient -c'


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

path=($HOME/.local/bin
	  $GOPATH/bin
	  $HOME/.cargo/bin
	  $HOME/.npm-global/bin
	  $HOME/bin
	  /usr/local/MacGPG2/bin(N-/)
	  /opt/homebrew/bin(N-/)
	  /Applications/Postgres.app/Contents/Versions/latest/bin(N-/)
	  /usr/local/{bin,sbin}
	  /usr/{bin,sbin}
	  /{bin,sbin}
	 )

# Remove any duplicates in $PATH and export.
typeset -gU path
export PATH
