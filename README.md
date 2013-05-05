Ross Timson's Dotfiles
======================

$HOME Sweet $HOME -- My dotfiles.

## Install

There is a Makefile that will symlink the dotfiles along with any extra
setup, read the Makefile for details and names of sub-tasks.  *Repo must
be cloned to `~/dotfiles`*

    git clone git://github.com/rosstimson/dotfiles.git ~/dotfiles
    cd ~/dotfiles
    make

## Test

There is a shell script in `bin/test_dotfiles`, the Makefile should use
this script to test it has succeeded.  It is also useful for showing
which symlinks are missing if only some of the dotfiles have been linked
manually or via a Make sub-task.

## Prerequisites

I primarily use FreeBSD and cli tools/software these days, however a lot
of these will still work on Mac and Linux as I originally started out on
those operating systems.

The dotfiles will expect certain software to be present which is listed
below.  *Note these are the name and locations in the FreeBSD ports
tree, other distros will differ.*

### Passwords

    sysutils/password-store

There are no passwords in these files, instead there are scripts which
pull in passwords via [Pass](http://zx2c4.com/projects/password-store/).
The files expect passwords in the following structure.

    Password Store
    └── AppSpecific
        ├── msmtp-gmail
        ├── msmtp-rosstimson
        ├── offlineimap-gmail
        └── offlineimap-rosstimson

### Email / Mutt
    graphics/feh
    print/gv
    mail/offlineimap
    mail/msmtp
    mail/notmuch

### Music
    audio/mpc
    audio/musicpd
    audio/ncmpcpp

### Vim
    devel/ctags
    devel/git
    lang/ruby20
    textproc/par
    textproc/the_silver_searcher

### Xorg
    graphics/scrot
    x11/xautolock
    x11/rxvt-unicode
    x11/numlockx
    x11-wm/openbox
    x11-wm/spectrwm

## Contact

Email: [ross@rosstimson.com](mailto:ross@rosstimson.com)

Twitter: [@rosstimson](http://twitter.com/rosstimson)
