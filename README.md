Ross Timson's Dotfiles
=======================

My config files for *nix systems including Vim and Emacs.  I predominately use Z shell (zsh) and the configs are geared towards Ruby on Rails development.  These should work fine on a Mac (for which they were originally developed), however I have recently switched to FreeBSD as my main machine.

Installation
------------

Clone the repository into your home directory with:

      $ git clone git://github.com/rosstimson/dotfiles.git

Next run the dotsetup.sh script to download all the Git submodules and symlink the dotfiles.

      $ ~/dotfiles/dotsetup.sh
      
__Note:__ REMEMBER to edit the following files to add in private data (there are placeholders in the files):
  + .gitconfig - add in Github token.
  + .muttrc - add in email account passwords.
      
Dependencies
------------

### Vim
  + Exuberant Ctags [Ctags](http://ctags.sourceforge.net/ "Ctags")
  + Ncurses-term (in Linux only)

### IRB
  + Uses [irbtools](https://github.com/janlelis/irbtools), install with:

      $ gem install irbtools  

Credits
-------

Much of the code here has been unashamedly pilfered from various sources and tweaked; I cannot remember everywhere I have found stuff used however the Vim and Emacs files are based on:

+  [Fabio Akita's (Akita on Rails) Vimfiles](http://github.com/akitaonrails/vimfiles "Akita on Rails Vimfiles")

+  [Geoffrey Grosenbachs (Topfunky) Emacs-Starter-Kit](http://github.com/topfunky/emacs-starter-kit "Topfunky's Emacs-Starter-Kit")

Contact
-------

Please feel free to contact me at: <ross@rosstimson.com>

Or on Twitter [@rosstimson](http://twitter.com/rosstimson)
