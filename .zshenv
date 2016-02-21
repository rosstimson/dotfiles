# ~/.zshenv

# Setting PATH
path=(
  /sbin
  /bin
  /usr/sbin
  /user/bin
  /usr/games
  /usr/local/sbin
  /usr/local/bin
  /root/bin
  "$HOME"/bin
)
export PATH
# Only unique entries please.
typeset -U path
# Remove entries that don't exist on this system.  Just for sanity's
# sake more than anything.
rationalize-path path

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
export PATH="$PYENV_ROOT/bin:$PATH"

# Go
export GOPATH="$HOME/code/go"
export PATH=$PATH:$GOPATH/bin

# Cask, Emacs dependency management made easy
export PATH=$PATH:$HOME/.cask/bin

