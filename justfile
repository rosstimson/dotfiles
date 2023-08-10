# ~/.dotfiles/justfile
# $HOME Sweet $HOME

# install
default: install


# stow (symlink) all dotfiles into $HOME
install:
	# Symlink all files with GNU Stow.
	stow -v -R --target=$HOME .

	# Some programs will complain or not work until certain directories or
	# permissions are present/set.
	mkdir -p ~/.mail/rosstimson.com
	chmod -R 700 ~/.mail
	chmod 600 ~/.msmtprc
	chmod 700 ~/.gnupg
	touch ~/.sh_history


# remove all stowed symlinks
uninstall:
	stow -D --target=$HOME .

# dry run of stow
dry:
	stow -v -R -n --target=$HOME .
