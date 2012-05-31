# ~/.zshenv

typeset -U path
for dir in /bin /usr/local/bin /opt/local/bin "$HOME/bin"; do
  [ -d "$dir" ] && path=($dir $path)
done
for dir in /usr/bin /usr/bin/X11 /usr/games /var/lib/gems/1.8/bin /usr/local/sbin /usr/sbin /sbin /usr/etc; do
  [ -d "$dir" ] && path=($path $dir)
done
# Usual suspects + ghar for managing dotfiles & rbenv for managing Rubies.
path=($HOME/bin $HOME/tools/ghar/bin $HOME/.rbenv/bin $path /usr/bin/X11 /usr/games /usr/local/sbin /usr/sbin /sbin)
# Enable rbenv shims.
eval "$(rbenv init -)"

# Timezone
export TZ='Europe/London'

# Setting Language / Locale
export LANG='en_GB.UTF-8'
export LC_ALL='C'

