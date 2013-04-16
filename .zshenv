# ~/.zshenv

typeset -U path
for dir in /bin /usr/local/bin /opt/local/bin "$HOME/bin"; do
  [ -d "$dir" ] && path=($dir $path)
done
for dir in /usr/bin /usr/bin/X11 /usr/games /var/lib/gems/1.8/bin /usr/local/sbin /usr/sbin /sbin /usr/etc; do
  [ -d "$dir" ] && path=($path $dir)
done
# Usual suspects & rbenv for managing Rubies.
path=($HOME/bin $path /usr/bin/X11 /usr/games /usr/local/sbin /usr/sbin /sbin)

# Timezone
export TZ='Europe/London'

# Setting Language / Locale
export LANG='en_GB.UTF-8'
export LC_COLLATE='C'

# Secure emails etc. with GPG
export GPGKEY=97865D41

# Go-lang variables
export GOOS=freebsd
export GOARCH=amd64
export GOROOT=/usr/local/go
