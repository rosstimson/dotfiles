#!/bin/sh -x

# Assumes repo has been cloned into home directory
cd ~/dotfiles

# Pull in submodules
echo Pulling in Vim, ZSH and Emacs submodules...
echo ===========================================
git submodule init
git submodule update

# Pull in submodules within Emacs
cd .emacs.d
echo Pulling in Emacs submodules...
echo ===========================================
git submodule init
git submodule update
cd -

# Symlink all of the dotfiles
echo Symlinking files...
echo ===========================================
for dotfile in .abcde.conf .config .conky .irssi .conkyrc .emacs.d .gemrc \
  .gitconfig .gitignore .gvimrc.before .gvimrc.after .hgrc .irbrc .janus \
  .jshintrc .muttrc .scripts .spectrwm.conf .tmux.conf .ttyerrc .vim \
  .vimrc.before .vimrc.after .Xdefaults .xinitrc .zsh .zshrc
do
  rm -fr ~/$dotfile
  ln -s $PWD/$dotfile ~/$dotfile
done

# Final message
echo Remember to add passwords to .gitconfig, .muttrc and .irssi/config.
echo Also remember to run rake in .vim to complete Janus install.
