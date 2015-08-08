# ~/dotfiles/Makefile
# $HOME Sweet $HOME

# List out all dotfiles except .git so that we can interatively copy them.
DOTFILES=$(shell ls -d .[^.]* | grep -v .git)
# Hardcoded home in case some config mngt too is installing.
RT_HOME='/home/rosstimson'

FZF_VERSION='0.10.2'

install:
	@for file in ${DOTFILES} ; do \
		cp -r $$file ${RT_HOME} ; \
	done

	# Some programs will complain or not work until certain directories or
	# permissions are not set
	@mkdir -p ${RT_HOME}/.mail/ross-rosstimson.com
	@mkdir -p ${RT_HOME}/.mail/rosstimson-gmail.com
	@chmod -R 700 ${RT_HOME}/.mail
	@chmod 600 ${RT_HOME}/.msmtprc
	@chmod 700 ${RT_HOME}/.gnupg

	# Download and install FZF without using the installer which messes with
	# dotfiles buy injecting lines into them.
	@curl -sfL https://github.com/junegunn/fzf-bin/releases/download/${FZF_VERSION}/fzf-${FZF_VERSION}-linux_amd64.tgz | tar -C ${RT_HOME}/.fzf/bin -xz
	@chmod +x ${RT_HOME}/.fzf/bin/fzf-${FZF_VERSION}-linux_amd64
	@ln -s ${RT_HOME}/.fzf/bin/fzf-${FZF_VERSION}-linux_amd64 ${RT_HOME}/.fzf/bin/fzf

	# Copy bin directory
	@cp -r bin ${RT_HOME}

.PHONY: install
