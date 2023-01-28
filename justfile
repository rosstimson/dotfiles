# install
default: install


# stow (symlink) all dotfiles into $HOME
install:
    stow --target=$HOME .

# remove all stowed symlinks
uninstall:
    stow -D .

# dry run of stow
dry:
    stow -n --target=$HOME .
