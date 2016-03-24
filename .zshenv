# -------------------------------------------------------------------------- {{{
#
# .zshenv: zsh environment settings
#

# Since .zshenv is sourced for every invocation of zsh, it is vital that
# it run quickly.  Only environment variables that must be set for
# a noninteractive shell are set here.  If external commands must be run
# here they are kept to a bare minimum and exported shell variables
# remember the results to keep them from needing to be run again.

# }}}

# zsh functions ------------------------------------------------------------ {{{

# Most functions are auto-loaded later on, but some are defined
# explicitly since they're needed by the startup files and would
# automatically be loaded in any case.

# rationalize-path()
# Later we'll need to trim down the paths that follow because the ones
# given here are for all my accounts, some of which have unusual
# paths in them.  rationalize-path will remove
# nonexistent directories from an array.
rationalize-path () {
  # Note that this works only on arrays, not colon-delimited strings.
  # Not that this is a problem now that there is typeset -T.
  local element
  local build
  build=()
  # Evil quoting to survive an eval and to make sure that
  # this works even with variables containing IFS characters, if I'm
  # crazy enough to setopt shwordsplit.
  eval '
  foreach element in "$'"$1"'[@]"
  do
    if [[ -d "$element" ]]
    then
      build=("$build[@]" "$element")
    fi
  done
  '"$1"'=( "$build[@]" )
  '
}

# }}}

# Shell options ------------------------------------------------------------ {{{

# These are the options that apply to noninteractive shells.
# Ones in capitals are variations from the default ZSH behaviour.
setopt \
  multios

# }}}

# Misc. vars --------------------------------------------------------------- {{{

# Timezone
export TZ='Europe/London'

# Setting Language / Locale
export LANG='en_GB.UTF-8'
export LC_COLLATE='C'

# Secure emails etc. with GPG
export GPGKEY=0x667A3481E4BB34F3

# Set Python path
export PYTHONPATH=/usr/lib/python2.7/site-packages

# pyenv settings
export PYENV_ROOT="$HOME/.pyenv"

# Go
export GOPATH="$HOME/code/go"

# }}}

# Paths for zsh ------------------------------------------------------------ {{{

# Path to search for autoloadable functions.
fpath=( $HOME/.zsh/lib "$fpath[@]" )
export FPATH
# Only unique entries please.
typeset -U fpath

# Include function path in script path so that we can run them even
# though a subshell may not know about functions.
# PATH should already be exported, but in case not. . .
path=(
  /sbin
  /usr/sbin
  /bin
  /usr/bin
  /usr/X11R6/bin
  /usr/local/sbin
  /usr/local/bin
  /usr/games
  "$HOME"/bin
  "$PYENV_ROOT/bin"         # Pyenv
  "$GOPATH/bin"             # Go binaries
  "$HOME/.cask/bin"         # Cask, Emacs dependency management
  "$path[@]"
  "$fpath[@]"
)
export PATH
# Only unique entries please.
typeset -U path
# Remove entries that don't exist on this system.  Just for sanity's
# sake more than anything.
rationalize-path path

# }}}
