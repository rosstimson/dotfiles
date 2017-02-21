"
" ~/.gvimrc
" My Vim (GUI) preference
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

set lines=999 columns=110           " Set initial size (max height)
set guifont=Hack\ 11                " Font
set visualbell                      " Don't beep
set t_vb=                           " Don't flash
set guioptions-=T                   " Remove toolbar, left & right scrollbar
set guioptions-=l
set guioptions-=L
set guioptions-=r
set guioptions-=R
set guioptions+=c                   " Use console dialogs
colorscheme base16-atelier-lakeside " Set colour scheme
