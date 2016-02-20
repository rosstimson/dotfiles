# ~/dotfiles/Makefile
# $HOME Sweet $HOME

# List out all dotfiles except .git so that we can interatively copy them.
DOTFILES=$(shell find . -maxdepth 1 -name '.[^.]*' -not -name '.git')
# Hardcoded home in case some config mngt tool is installing.
RT_HOME='/home/rosstimson'

FZF_VERSION='0.11.3'

install:
	@for file in ${DOTFILES} ; do \
		cp -a $$file ${RT_HOME} ; \
	done

# Some programs will complain or not work until certain directories or
# permissions are present/set.
	@mkdir -p ${RT_HOME}/.mail/ross-rosstimson.com
	@mkdir -p ${RT_HOME}/.mail/rosstimson-gmail.com
	@chmod -R 700 ${RT_HOME}/.mail
	@chmod 600 ${RT_HOME}/.msmtprc
	@chmod 700 ${RT_HOME}/.gnupg

# Download and install FZF without using the installer which messes with
# dotfiles buy injecting lines into them.
	git clone https://github.com/junegunn/fzf.git ${RT_HOME}/.fzf
	@curl -sfL https://github.com/junegunn/fzf-bin/releases/download/${FZF_VERSION}/fzf-${FZF_VERSION}-linux_amd64.tgz | tar xzvf - -C ${RT_HOME}/.fzf/bin
	@chmod +x ${RT_HOME}/.fzf/bin/fzf-${FZF_VERSION}-linux_amd64
	@ln -sf ${RT_HOME}/.fzf/bin/fzf-${FZF_VERSION}-linux_amd64 ${RT_HOME}/.fzf/bin/fzf

# Copy bin directory
	@cp -r bin ${RT_HOME}

clean:
	@for file in ${DOTFILES} ; do \
		rm -rf ${RT_HOME}/$$file ; \
	done

.PHONY: install clean
