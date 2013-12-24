# ~/dotfiles/Makefile
# $HOME Sweet $HOME

DOTFILES=`pwd`
RT_RT_HOME='/home/rosstimson'

all: 	developer-install emacs-install email-install general-install \
			gpg-agent-install misc-install music-install scripts-install \
			shells-install vcs-install vim-install xorg-install run-tests

clean: 	developer-remove emacs-remove email-remove general-remove \
				gpg-agent-remove misc-remove music-remove scripts-remove shells-remove \
				vcs-remove vim-remove xorg-remove

developer-install:
	ln -sfn ${DOTFILES}/.ackrc ${RT_HOME}/.ackrc
	ln -sfn ${DOTFILES}/.gemrc ${RT_HOME}/.gemrc
	ln -sfn ${DOTFILES}/.irbrc ${RT_HOME}/.irbrc
	ln -sfn ${DOTFILES}/.jshintrc ${RT_HOME}/.jshintrc
	ln -sfn ${DOTFILES}/.porttools ${RT_HOME}/.porttools

developer-remove:
	-@rm -f ${RT_HOME}/.ackrc
	-@rm -f ${RT_HOME}/.gemrc
	-@rm -f ${RT_HOME}/.irbrc
	-@rm -f ${RT_HOME}/.jshintrc
	-@rm -f ${RT_HOME}/.porttools

emacs-install:
	ln -sfn ${DOTFILES}/.emacs.d ${RT_HOME}/.emacs.d

emacs-remove:
	-@rm -f ${RT_HOME}/.emacs.d

email-install:
	mkdir -p ${RT_HOME}/.mail/ross-rosstimson.com
	mkdir -p ${RT_HOME}/.mail/rosstimson-gmail.com
	chmod -R 700 ${RT_HOME}/.mail
	ln -sfn ${DOTFILES}/.mailcap ${RT_HOME}/.mailcap
	ln -sfn ${DOTFILES}/.msmtprc ${RT_HOME}/.msmtprc
	chmod 600 ${RT_HOME}/.msmtprc
	ln -sfn ${DOTFILES}/.mutt ${RT_HOME}/.mutt
	ln -sfn ${DOTFILES}/.muttrc ${RT_HOME}/.muttrc
	ln -sfn ${DOTFILES}/.notmuch-config ${RT_HOME}/.notmuch-config
	ln -sfn ${DOTFILES}/.offlineimaprc ${RT_HOME}/.offlineimaprc
	ln -sfn ${DOTFILES}/.urlview ${RT_HOME}/.urlview

email-remove:
	-@echo "Not blitzing email store ${RT_HOME}/.mail"
	-@rm -f ${RT_HOME}/.mailcap
	-@rm -f ${RT_HOME}/.msmtprc
	-@rm -f ${RT_HOME}/.mutt
	-@rm -f ${RT_HOME}/.muttrc
	-@rm -f ${RT_HOME}/.notmuch-config
	-@rm -f ${RT_HOME}/.offlineimaprc
	-@rm -f ${RT_HOME}/.urlview

general-install:
	ln -sfn ${DOTFILES}/.cshrc ${RT_HOME}/.cshrc
	ln -sfn ${DOTFILES}/.exrc ${RT_HOME}/.exrc
	ln -sfn ${DOTFILES}/.fonts ${RT_HOME}/.fonts
	ln -sfn ${DOTFILES}/.login ${RT_HOME}/.login
	ln -sfn ${DOTFILES}/.login_conf ${RT_HOME}/.login_conf
	ln -sfn ${DOTFILES}/.profile ${RT_HOME}/.profile
	ln -sfn ${DOTFILES}/.shrc ${RT_HOME}/.shrc
	ln -sfn ${DOTFILES}/.termcap ${RT_HOME}/.termcap

general-remove:
	-@rm -f ${RT_HOME}/.cshrc
	-@rm -f ${RT_HOME}/.exrc
	-@rm -f ${RT_HOME}/.fonts
	-@rm -f ${RT_HOME}/.login
	-@rm -f ${RT_HOME}/.login_conf
	-@rm -f ${RT_HOME}/.profile
	-@rm -f ${RT_HOME}/.shrc
	-@rm -f ${RT_HOME}/.termcap

gpg-agent-install:
	[ -d ${RT_HOME}/.gnupg ] || mkdir -v -m 700 ${RT_HOME}/.gnupg
	ln -sfvn ${DOTFILES}/.gpg-agent.conf ${RT_HOME}/.gnupg/.gpg-agent.conf

gpg-agent-remove:
	-@echo "Not blitzing ${RT_HOME}/.gnupg directory in case there are keys in it."
	-@rm -f ${RT_HOME}/.gnupg/.gpg-agent.conf

misc-install:
	ln -sfn ${DOTFILES}/.irssi ${RT_HOME}/.irssi
	ln -sfn ${DOTFILES}/.tarignore ${RT_HOME}/.tarignore
	ln -sfn ${DOTFILES}/.tmux.conf ${RT_HOME}/.tmux.conf
	ln -sfn ${DOTFILES}/.ttytterrc ${RT_HOME}/.ttytterrc

misc-remove:
	-@rm -f ${RT_HOME}/.irssi
	-@rm -f ${RT_HOME}/.tarignore
	-@rm -f ${RT_HOME}/.tmux.conf
	-@rm -f ${RT_HOME}/.ttytterrc

music-install:
	ln -sfn ${DOTFILES}/.abcde.conf ${RT_HOME}/.abcde.conf
	ln -sfn ${DOTFILES}/.mpdconf ${RT_HOME}/.mpdconf
	ln -sfn ${DOTFILES}/.ncmpcpp ${RT_HOME}/.ncmpcpp

music-remove:
	-@rm -f ${RT_HOME}/.abcde.conf
	-@rm -f ${RT_HOME}/.mpdconf
	-@rm -f ${RT_HOME}/.ncmpcpp

scripts-install:
	ln -sfn ${DOTFILES}/bin ${RT_HOME}/bin
	ln -sfn ${DOTFILES}/.urxvt-perls ${RT_HOME}/.urxvt-perls

scripts-remove:
	-@rm -f ${RT_HOME}/bin
	-@rm -f ${RT_HOME}/.urxvt-perls

shells-install:
	ln -sfn ${DOTFILES}/.inputrc ${RT_HOME}/.inputrc
	ln -sfn ${DOTFILES}/.zsh ${RT_HOME}/.zsh
	ln -sfn ${DOTFILES}/.zsh_nocorrect ${RT_HOME}/.zsh_nocorrect
	ln -sfn ${DOTFILES}/.zshenv ${RT_HOME}/.zshenv
	ln -sfn ${DOTFILES}/.zshrc ${RT_HOME}/.zshrc

shells-remove:
	-@rm -f ${RT_HOME}/.inputrc
	-@rm -f ${RT_HOME}/.zsh
	-@rm -f ${RT_HOME}/.zsh_nocorrect
	-@rm -f ${RT_HOME}/.zshenv
	-@rm -f ${RT_HOME}/.zshrc

vcs-install:
	ln -sfn ${DOTFILES}/.bazaar ${RT_HOME}/.bazaar
	ln -sfn ${DOTFILES}/.gitconfig ${RT_HOME}/.gitconfig
	ln -sfn ${DOTFILES}/.gitignore ${RT_HOME}/.gitignore
	ln -sfn ${DOTFILES}/.hgignore ${RT_HOME}/.hgignore
	ln -sfn ${DOTFILES}/.hgrc ${RT_HOME}/.hgrc

vcs-remove:
	-@rm -f ${RT_HOME}/.bazaar
	-@rm -f ${RT_HOME}/.gitconfig
	-@rm -f ${RT_HOME}/.gitignore
	-@rm -f ${RT_HOME}/.hgignore
	-@rm -f ${RT_HOME}/.hgrc

vim-install:
	git submodule update --init
	ln -sfn ${DOTFILES}/.vim ${RT_HOME}/.vim
	ln -sfn ${DOTFILES}/.vimrc ${RT_HOME}/.vimrc
	ln -sfn ${DOTFILES}/.gvimrc ${RT_HOME}/.gvimrc

vim-remove:
	-@rm -f ${RT_HOME}/.vim
	-@rm -f ${RT_HOME}/.vimrc
	-@rm -f ${RT_HOME}/.gvimrc

xorg-install:
	ln -sfn ${DOTFILES}/.Xcolors ${RT_HOME}/.Xcolors
	ln -sfn ${DOTFILES}/.Xresources ${RT_HOME}/.Xresources
	ln -sfn ${DOTFILES}/.fonts.conf ${RT_HOME}/.fonts.conf
	ln -sfn ${DOTFILES}/.spectrwm.conf ${RT_HOME}/.spectrwm.conf
	ln -sfn ${DOTFILES}/.xinitrc ${RT_HOME}/.xinitrc
	ln -sfn ${DOTFILES}/.xserverrc ${RT_HOME}/.xserverrc

xorg-remove:
	-@rm -f ${RT_HOME}/.Xcolors
	-@rm -f ${RT_HOME}/.Xresources
	-@rm -f ${RT_HOME}/.fonts.conf
	-@rm -f ${RT_HOME}/.spectrwm.conf
	-@rm -f ${RT_HOME}/.xinitrc
	-@rm -f ${RT_HOME}/.xserverrc

run-tests:
	${DOTFILES}/bin/test_dotfiles


.PHONY: all clean
