# ~/dotfiles/Makefile
# $HOME Sweet $HOME

DOTFILES=`pwd`

all: 	developer-install email-install general-install gpg-agent-install \
			misc-install music-install scripts-install shells-install vcs-install \
			vim-install xorg-install run-tests

clean: 	developer-remove email-remove general-remove gpg-agent-remove \
				misc-remove music-remove scripts-remove shells-remove vcs-remove \
				vim-remove xorg-remove

developer-install:
	ln -sfn ${DOTFILES}/.ackrc ${HOME}/.ackrc
	ln -sfn ${DOTFILES}/.gemrc ${HOME}/.gemrc
	ln -sfn ${DOTFILES}/.irbrc ${HOME}/.irbrc
	ln -sfn ${DOTFILES}/.jshintrc ${HOME}/.jshintrc
	ln -sfn ${DOTFILES}/.porttools ${HOME}/.porttools
	ln -sfn ${DOTFILES}/.smrc ${HOME}/.smrc

developer-remove:
	-@rm -f ${HOME}/.ackrc
	-@rm -f ${HOME}/.gemrc
	-@rm -f ${HOME}/.irbrc
	-@rm -f ${HOME}/.jshintrc
	-@rm -f ${HOME}/.porttools
	-@rm -f ${HOME}/.smrc

email-install:
	mkdir -p ${HOME}/.mail/ross-rosstimson.com
	mkdir -p ${HOME}/.mail/rosstimson-gmail.com
	chmod -R 700 ${HOME}/.mail
	ln -sfn ${DOTFILES}/.mailcap ${HOME}/.mailcap
	ln -sfn ${DOTFILES}/.msmtprc ${HOME}/.msmtprc
	chmod 600 ${HOME}/.msmtprc # MSMTP will not work until correct permissions set.
	ln -sfn ${DOTFILES}/.mutt ${HOME}/.mutt
	ln -sfn ${DOTFILES}/.muttrc ${HOME}/.muttrc
	ln -sfn ${DOTFILES}/.notmuch-config ${HOME}/.notmuch-config
	ln -sfn ${DOTFILES}/.offlineimaprc ${HOME}/.offlineimaprc
	ln -sfn ${DOTFILES}/.urlview ${HOME}/.urlview

email-remove:
	-@echo "Not blitzing email store ${HOME}/.mail"
	-@rm -f ${HOME}/.mailcap
	-@rm -f ${HOME}/.msmtprc
	-@rm -f ${HOME}/.mutt
	-@rm -f ${HOME}/.muttrc
	-@rm -f ${HOME}/.notmuch-config
	-@rm -f ${HOME}/.offlineimaprc
	-@rm -f ${HOME}/.urlview

general-install:
	ln -sfn ${DOTFILES}/.cshrc ${HOME}/.cshrc
	ln -sfn ${DOTFILES}/.exrc ${HOME}/.exrc
	ln -sfn ${DOTFILES}/.login ${HOME}/.login
	ln -sfn ${DOTFILES}/.login_conf ${HOME}/.login_conf
	ln -sfn ${DOTFILES}/.profile ${HOME}/.profile
	ln -sfn ${DOTFILES}/.shrc ${HOME}/.shrc
	ln -sfn ${DOTFILES}/.termcap ${HOME}/.termcap

general-remove:
	-@rm -f ${HOME}/.cshrc
	-@rm -f ${HOME}/.exrc
	-@rm -f ${HOME}/.login
	-@rm -f ${HOME}/.login_conf
	-@rm -f ${HOME}/.profile
	-@rm -f ${HOME}/.shrc
	-@rm -f ${HOME}/.termcap

gpg-agent-install:
	[ -d ${HOME}/.gnupg ] || mkdir -v -m 700 ${HOME}/.gnupg
	ln -sfvn ${DOTFILES}/.gpg-agent.conf ${HOME}/.gnupg/.gpg-agent.conf

gpg-agent-remove:
	-@echo "Not blitzing ${HOME}/.gnupg directory in case there are keys in it."
	-@rm -f ${HOME}/.gnupg/.gpg-agent.conf

misc-install:
	ln -sfn ${DOTFILES}/.irssi ${HOME}/.irssi
	ln -sfn ${DOTFILES}/.tmux.conf ${HOME}/.tmux.conf

misc-remove:
	-@rm -f ${HOME}/.irssi
	-@rm -f ${HOME}/.tmux.conf

music-install:
	ln -sfn ${DOTFILES}/.abcde.conf ${HOME}/.abcde.conf
	ln -sfn ${DOTFILES}/.mpdconf ${HOME}/.mpdconf
	ln -sfn ${DOTFILES}/.ncmpcpp ${HOME}/.ncmpcpp

music-remove:
	-@rm -f ${HOME}/.abcde.conf
	-@rm -f ${HOME}/.mpdconf
	-@rm -f ${HOME}/.ncmpcpp

scripts-install:
	ln -sfn ${DOTFILES}/bin ${HOME}/bin
	ln -sfn ${DOTFILES}/.urxvt-perls ${HOME}/.urxvt-perls

scripts-remove:
	-@rm -f ${HOME}/bin
	-@rm -f ${HOME}/.urxvt-perls

shells-install:
	git submodule update --init # Grab oh-my-zsh first.
	ln -sfn ${DOTFILES}/.oh-my-zsh ${HOME}/.oh-my-zsh
	ln -sfn ${DOTFILES}/.zprofile ${HOME}/.zprofile
	ln -sfn ${DOTFILES}/.zsh_nocorrect ${HOME}/.zsh_nocorrect
	ln -sfn ${DOTFILES}/.zshenv ${HOME}/.zshenv
	ln -sfn ${DOTFILES}/.zshrc ${HOME}/.zshrc

shells-remove:
	-@rm -f ${HOME}/.oh-my-zsh
	-@rm -f ${HOME}/.zprofile
	-@rm -f ${HOME}/.zsh_nocorrect
	-@rm -f ${HOME}/.zshenv
	-@rm -f ${HOME}/.zshrc

vcs-install:
	ln -sfn ${DOTFILES}/.bazaar ${HOME}/.bazaar
	ln -sfn ${DOTFILES}/.gitconfig ${HOME}/.gitconfig
	ln -sfn ${DOTFILES}/.gitignore ${HOME}/.gitignore
	ln -sfn ${DOTFILES}/.hgignore ${HOME}/.hgignore
	ln -sfn ${DOTFILES}/.hgrc ${HOME}/.hgrc

vcs-remove:
	-@rm -f ${HOME}/.bazaar
	-@rm -f ${HOME}/.gitconfig
	-@rm -f ${HOME}/.gitignore
	-@rm -f ${HOME}/.hgignore
	-@rm -f ${HOME}/.hgrc

vim-install:
	git submodule update --init # Grab extra plugins not included in Janus
	ln -sfn ${DOTFILES}/.janus ${HOME}/.janus
	ln -sfn ${DOTFILES}/.vimrc.before ${HOME}/.vimrc.before
	ln -sfn ${DOTFILES}/.vimrc.after ${HOME}/.vimrc.after
	ln -sfn ${DOTFILES}/.gvimrc.before ${HOME}/.gvimrc.before
	ln -sfn ${DOTFILES}/.gvimrc.after ${HOME}/.gvimrc.after

vim-remove:
	-@rm -f ${HOME}/.janus
	-@rm -f ${HOME}/.vimrc.before
	-@rm -f ${HOME}/.vimrc.after
	-@rm -f ${HOME}/.gvimrc.before
	-@rm -f ${HOME}/.gvimrc.after

xorg-install:
	ln -sfn ${DOTFILES}/.Xcolors ${HOME}/.Xcolors
	ln -sfn ${DOTFILES}/.Xresources ${HOME}/.Xresources
	ln -sfn ${DOTFILES}/.fonts.conf ${HOME}/.fonts.conf
	ln -sfn ${DOTFILES}/.spectrwm.conf ${HOME}/.spectrwm.conf
	ln -sfn ${DOTFILES}/.xinitrc ${HOME}/.xinitrc
	ln -sfn ${DOTFILES}/.xserverrc ${HOME}/.xserverrc

xorg-remove:
	-@rm -f ${HOME}/.Xcolors
	-@rm -f ${HOME}/.Xresources
	-@rm -f ${HOME}/.fonts.conf
	-@rm -f ${HOME}/.spectrwm.conf
	-@rm -f ${HOME}/.xinitrc
	-@rm -f ${HOME}/.xserverrc

run-tests:
	${DOTFILES}/bin/test_dotfiles


.PHONY: all clean
