Ross Timson's Dotfiles
======================

$HOME Sweet $HOME -- My dotfiles.

## Install

There is a Makefile that will symlink the dotfiles along with any extra
setup, read the Makefile for details and names of sub-tasks.

    git clone git://github.com/rosstimson/dotfiles.git ~/code/dotfiles
    cd ~/code/dotfiles
    make

## Expected Software

The dotfiles will expect certain utility tools to be installed.

* [fzy](https://github.com/jhawthorn/fzy)
* [pass](https://www.passwordstore.org)
* [ripgrep](https://github.com/BurntSushi/ripgrep)
* [xclip](https://github.com/astrand/xclip)
* [pretty-git-prompt](https://github.com/TomasTomecek/pretty-git-prompt)

### Emacs

Various Emacs modes expect certain tools to be installed.

* MultiMarkdown

## Test

There is a shell script in `bin/test_dotfiles`, the Makefile should use
this script to test it has succeeded.  It is also useful for showing
which symlinks are missing if only some of the dotfiles have been linked
manually or via a Make sub-task.

