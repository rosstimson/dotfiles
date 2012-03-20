# Path to your oh-my-zsh configuration.
export ZSH=$HOME/dotfiles/.zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="rosstimson"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(brew bundler gem git github rails3 ruby)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

###  My stuff:

# Timezone
export TZ='Europe/London'

# Setting Language / Locale
export LANG='en_US.UTF-8'
export LC_ALL='C'

# Coloured ManPages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# History
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000

setopt append_history
setopt share_history
setopt extended_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_no_store

# Set an alias for 'ls'
case `uname` in
	FreeBSD)
		alias ls='gnuls --color=auto -GF'
	;;
	NetBSD)
		alias ls='ls -F'
	;;
	OpenBSD)
		alias ls='colorls -GF'
	;;
	Linux)
		alias ls='ls --color=always -F'
	;;
esac

# Setting Term
#export TERM=rxvt-unicode

# Set PATH, CDPATH, EDITOR, etc.
export EDITOR='vim -f'
export PAGER='less -R'
export PATH='/usr/bin:/bin:/usr/sbin:/sbin'
export PATH='/usr/local/bin:/usr/local/sbin:$PATH'
export PATH='/usr/local/mysql/bin:$PATH'
export PATH='$HOME/bin:$PATH'
# Setting path for NodeJS libs.  
# npm ls -g will show you where this should be (add node_modules to output).
export NODE_PATH='/usr/local/lib/node_modules'

# Added for RVM (Ruby Version Manager)
if [[ -s $HOME/.rvm/scripts/rvm ]] ; then source $HOME/.rvm/scripts/rvm ; fi

# Added for Tmuxinator
if [[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] ; then source $HOME/.tmuxinator/scripts/tmuxinator ; fi

# Added for using GPG secured emails
export GPGKEY=97865D41

# Other misc settings
LISTMAX=0

# Needed for using Janus alongside RVM:
# https://github.com/carlhuda/janus/wiki/Rvm
#
# Define Vim wrappers which unsets GEM_HOME and GEM_PATH before
# invoking vim and all known aliases
#
# @author Wael Nasreddine <wael.nasreddine@gmail.com>
function define_vim_wrappers()
{
  vim_commands=(
    eview evim gview gvim gvimdiff gvimtutor rgview
    rgvim rview rvim vim vimdiff vimtutor xxd mvim
  )

  for cmd in ${vim_commands[@]}; do
    cmd_path=`/usr/bin/env which -a "${cmd}" 2>/dev/null | grep '^/'`
    if [ -x "${cmd_path}" ]; then
      eval "function ${cmd} () { (unset GEM_HOME; unset GEM_PATH; $cmd_path \$@) }"
    fi
  done
}

# Call function defined above.
define_vim_wrappers
