"
" ~/.vimrc
" My Vim preference
" Maintained by Ross Timson <ross@rosstimson.com>
"
" I like to build the latest Vim from src with the following:
"
"     ./configure --with-features=huge \
"                 --prefix=/usr/local \
"                 --enable-gui=yes \
"                 --with-x \
"                 --enable-pythoninterp \
"                 --enable-multibyte \
"                 --enable-fontset \
"                 --enable-cscope \
"                 --disable-netbeans \
"                 --with-compiledby="Ross Timson <ross@rosstimson.com>"
"
" To start vim without using this .vimrc file, use:
"     vim -u NORC
"
" To start vim without loading any .vimrc or plugins, use:
"     vim -u NONE
"

set nocompatible 	" be iMproved (MUST be first)
filetype off 		" Required !

" Use Vundle to for installing / managing Vim scripts.

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" ------------------------------------------------------------------------------
" Bundles

" Let Vundle manage Vundle (ooh how meta)
Bundle 'gmarik/vundle'

" Tools
Bundle 'tpope/vim-fugitive'
Bundle 'kien/ctrlp.vim'



filetype plugin indent on
