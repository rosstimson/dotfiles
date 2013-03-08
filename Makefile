# ~/dotfiles/Makefile
# $HOME Sweet $HOME

DOTFILES=`pwd`

all: 	developer-install email-install general-install gpg-agent-install \
			misc-install music-install scripts-install shells-install vcs-install \
			vim-install xorg-install

clean: 	developer-remove email-remove general-remove gpg-agent-remove \
				misc-remove music-remove scripts-remove shells-remove vcs-remove \
				vim-remove xorg-remove

developer-install:
	ln -sfvn ${DOTFILES}/.ackrc ~/.ackrc
	ln -sfvn ${DOTFILES}/.gemrc ~/.gemrc
	ln -sfvn ${DOTFILES}/.irbrc ~/.irbrc
	ln -sfvn ${DOTFILES}/.jshintrc ~/.jshintrc
	ln -sfvn ${DOTFILES}/.porttools ~/.porttools
	ln -sfvn ${DOTFILES}/.smrc ~/.smrc

developer-remove:
	-@rm -f ~/.ackrc
	-@rm -f ~/.gemrc
	-@rm -f ~/.irbrc
	-@rm -f ~/.jshintrc
	-@rm -f ~/.porttools
	-@rm -f ~/.smrc

email-install:
	# TODO Alias INBOX of boths account in ~/.mail so they are named nicely in
	# sidebar. Create ~/.mail directories with correct perms and symlink INBOX.
	ln -sfvn ${DOTFILES}/.mailcap ~/.mailcap
	ln -sfvn ${DOTFILES}/.msmtprc ~/.msmtprc
	chmod 600 ~/.msmtprc # MSMTP will not work until correct permissions set.
	ln -sfvn ${DOTFILES}/.mutt ~/.mutt
	ln -sfvn ${DOTFILES}/.muttrc ~/.muttrc
	ln -sfvn ${DOTFILES}/.notmuch-config ~/.notmuch-config
	ln -sfvn ${DOTFILES}/.offlineimaprc ~/.offlineimaprc
	ln -sfvn ${DOTFILES}/.urlview ~/.urlview

email-remove:
	-@rm -f ~/.mailcap
	-@rm -f ~/.msmtprc
	-@rm -f ~/.mutt
	-@rm -f ~/.muttrc
	-@rm -f ~/.notmuch-config
	-@rm -f ~/.offlineimaprc
	-@rm -f ~/.urlview

general-install:
	ln -sfvn ${DOTFILES}/.cshrc ~/.cshrc
	ln -sfvn ${DOTFILES}/.exrc ~/.exrc
	ln -sfvn ${DOTFILES}/.login ~/.login
	ln -sfvn ${DOTFILES}/.login_conf ~/.login_conf
	ln -sfvn ${DOTFILES}/.profile ~/.profile
	ln -sfvn ${DOTFILES}/.shrc ~/.shrc
	ln -sfvn ${DOTFILES}/.termcap ~/.termcap

general-remove:
	-@rm -f ~/.cshrc
	-@rm -f ~/.exrc
	-@rm -f ~/.login
	-@rm -f ~/.login_conf
	-@rm -f ~/.profile
	-@rm -f ~/.shrc
	-@rm -f ~/.termcap

gpg-agent-install:
	[ -d ~/.gnupg ] || mkdir -m 700 ~/.gnupg
	ln -sfvn ${DOTFILES}/.gpg-agent.conf ~/.gnupg/.gpg-agent.conf

# Not blitzing ~/.gnupg directory that got created incase there are keys
# etc. in it.
gpg-agent-remove:
	-@rm -f ~/.gnupg/.gpg-agent.conf

misc-install:
	ln -sfvn ${DOTFILES}/.irssi ~/.irssi
	ln -sfvn ${DOTFILES}/.tmux.conf ~/.tmux.conf

misc-remove:
	-@rm -f ~/.irssi
	-@rm -f ~/.tmux.conf

music-install:
	ln -sfvn ${DOTFILES}/.abcde.conf ~/.abcde.conf
	ln -sfvn ${DOTFILES}/.mpdconf ~/.mpdconf
	ln -sfvn ${DOTFILES}/.ncmpcpp ~/.ncmpcpp

music-remove:
	-@rm -f ~/.abcde.conf
	-@rm -f ~/.mpdconf
	-@rm -f ~/.ncmpcpp

scripts-install:
	ln -sfvn ${DOTFILES}/bin ~/bin
	ln -sfvn ${DOTFILES}/.urxvt-perls ~/.urxvt-perls

scripts-remove:
	-@rm -f ${DOTFILES}/bin ~/bin
	-@rm -f ${DOTFILES}/.urxvt-perls ~/.urxvt-perls

shells-install:
	git submodule update --init # Grab oh-my-zsh first.
	ln -sfvn ${DOTFILES}/.oh-my-zsh ~/.oh-my-zsh
	ln -sfvn ${DOTFILES}/.zprofile ~/.zprofile
	ln -sfvn ${DOTFILES}/.zsh_nocorrect ~/.zsh_nocorrect
	ln -sfvn ${DOTFILES}/.zshenv ~/.zshenv
	ln -sfvn ${DOTFILES}/.zshrc ~/.zshrc

shells-remove:
	-@rm -f ~/.oh-my-zsh
	-@rm -f ~/.zprofile
	-@rm -f ~/.zsh_nocorrect
	-@rm -f ~/.zshenv
	-@rm -f ~/.zshrc

vcs-install:
	ln -sfvn ${DOTFILES}/.bazaar ~/.bazaar
	ln -sfvn ${DOTFILES}/.gitconfig ~/.gitconfig
	ln -sfvn ${DOTFILES}/.gitignore ~/.gitignore
	ln -sfvn ${DOTFILES}/.hgignore ~/.hgignore
	ln -sfvn ${DOTFILES}/.hgrc ~/.hgrc

vcs-remove:
	-@rm -f ~/.bazaar
	-@rm -f ~/.gitconfig
	-@rm -f ~/.gitignore
	-@rm -f ~/.hgignore
	-@rm -f ~/.hgrc

vim-install:
	git submodule update --init # Grab extra plugins not included in Janus
	ln -sfvn ${DOTFILES}/.janus ~/.janus
	ln -sfvn ${DOTFILES}/.vimrc.before ~/.vimrc.before
	ln -sfvn ${DOTFILES}/.vimrc.after ~/.vimrc.after
	ln -sfvn ${DOTFILES}/.gvimrc.before ~/.gvimrc.before
	ln -sfvn ${DOTFILES}/.gvimrc.after ~/.gvimrc.after

vim-remove:
	-@rm -f ~/.janus
	-@rm -f ~/.vimrc.before
	-@rm -f ~/.vimrc.after
	-@rm -f ~/.gvimrc.before
	-@rm -f ~/.gvimrc.after

xorg-install:
	ln -sfvn ${DOTFILES}/.Xcolors ~/.Xcolors
	ln -sfvn ${DOTFILES}/.Xresources ~/.Xresources
	ln -sfvn ${DOTFILES}/.fonts.conf ~/.fonts.conf
	ln -sfvn ${DOTFILES}/.spectrwm.conf ~/.spectrwm.conf
	ln -sfvn ${DOTFILES}/.xinitrc ~/.xinitrc
	ln -sfvn ${DOTFILES}/.xserverrc ~/.xserverrc

xorg-remove:
	-@rm -f ~/.Xcolors
	-@rm -f ~/.Xresources
	-@rm -f ~/.fonts.conf
	-@rm -f ~/.spectrwm.conf
	-@rm -f ~/.xinitrc
	-@rm -f ~/.xserverrc


# Use shunit2 to run some tests.
run-tests:
	./bin/test_dotfiles


.PHONY: all clean
