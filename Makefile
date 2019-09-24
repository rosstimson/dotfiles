# ~/dotfiles/Makefile
# $HOME Sweet $HOME

# List out all dotfiles except .git so that we can iteratively copy them.
DOTFILES != find . -maxdepth 1 -name '.[^.]*' ! -name '.git'
# Hardcoded home in case some config mngt tool is installing.
RT_HOME='/home/rosstimson'

install:
	@for file in ${DOTFILES} ; do \
		cp -pPR $$file ${RT_HOME} ; \
	done

# Some programs will complain or not work until certain directories or
# permissions are present/set.
	@mkdir -p ${RT_HOME}/.mail/rosstimson.com
	@chmod -R 700 ${RT_HOME}/.mail
	@chmod 600 ${RT_HOME}/.msmtprc
	@chmod 700 ${RT_HOME}/.gnupg
	@touch ${RT_HOME}/.sh_history

# Copy bin directory
	@cp -r bin ${RT_HOME}

clean:
	@for file in ${DOTFILES} ; do \
		rm -rf ${RT_HOME}/$$file ; \
	done

.PHONY: install clean
