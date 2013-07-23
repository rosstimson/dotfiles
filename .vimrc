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
"                 --enable-rubyinterp \
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

set nocompatible  " be iMproved (MUST be first)
filetype off      " Required !

" Use Vundle to for installing / managing Vim scripts

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()


" -----------------------------------------------------------------------------
" Bundles

" Let Vundle manage Vundle (ooh how meta)
Bundle 'gmarik/vundle'

" Tools
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-dispatch'
Bundle 'mileszs/ack.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'Lokaltog/vim-powerline'
Bundle 'LustyJuggler'
Bundle 'SirVer/ultisnips'
Bundle 'mattn/gist-vim'
Bundle 'vimwiki'
Bundle 'majutsushi/tagbar'
Bundle 'ZoomWin'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
Bundle 'gnupg'
Bundle 'nelstrom/vim-markdown-folding'
Bundle 'joonty/vim-phpqa'
Bundle 'joonty/vdebug'

" Langs
Bundle 'tpope/vim-git'
Bundle 'kchmck/vim-coffee-script'
Bundle 'tpope/vim-git'
Bundle 'jnwhiteh/vim-golang'
Bundle 'nono/vim-handlebars'
Bundle 'pangloss/vim-javascript'
Bundle 'tpope/vim-markdown'
Bundle 'sunaku/vim-ruby-minitest'
Bundle 'tpope/vim-rails'
Bundle 'skwp/vim-rspec'
Bundle 'vim-ruby/vim-ruby'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'lotheac/pf.vim'
Bundle 'nginx.vim'
Bundle 'othree/html5.vim'
Bundle 'rosstimson/bats.vim'
Bundle 'rosstimson/modx.vim'
Bundle 'php.vim'


" Colour schemes
Bundle 'twerth/ir_black'


" -----------------------------------------------------------------------------
" Basic settings

filetype plugin indent on " Required! Enable detection, plugins and indenting
set number                " Show line number
set ruler                 " Show line and column numbers
syntax enable             " Turn on syntax highlighting
set termencoding=utf-8    " Set term encoding to UTF-*
set encoding=utf-8        " Set default encoding to UTF-8
let mapleader=","         " Change leader key from \ to ,
let maplocalleader="\\"   " Set local leader to \\
set hidden                " Hide buffers instead of closing
set showmatch             " Show matching parantesis
set nobackup              " Don't keep backup files, it's 70's style cluttering
set noswapfile            " Don't write annoying intermediate swap files,
                          "   who did ever restore from swap files anyway?
set viminfo='20,\"80      " Read / Write a .viminfo file, don't store more
                          "   than 80 lines of registers
set title                 " Change the terminal's title
set visualbell            " Don't beep
set noerrorbells          " Don't beep
set showcmd               " Show (partial) command in the last line of the
                          "   screen this also shows visual selection info
set nomodeline            " Disable mode lines (security measure)
set cursorline            " Underline the current line, for quick orientation
set history=1000          " Remember more commands and search history
set undolevels=1000       " Use many muchos levels of undo
set scrolloff=4           " Keep 4 lines off the edges of the screen when
                          "   scrolling
set splitbelow            " Open horizontal splits on bottom (more natural)
set splitright            " Open vertical splits on right (more natural)
set colorcolumn=80        " Colour column 80 as a visual guide
set formatprg=par         " Use Par program to format text
set lazyredraw            " Don't update the display while executing macros
set laststatus=2          "   tell VIM to always put a status line in, even
                          "   if there is only one window
set cmdheight=2           " Use a status bar that is 2 rows high
set pastetoggle=<F2>      " When in insert mode, press <F2> to go to
                          "   paste mode, where you can paste mass data that
                          "   won't be autoindented


" -----------------------------------------------------------------------------
"  Whitespace

set nowrap                        " Don't wrap lines
set tabstop=2                     " A tab is 2 spaces
set shiftwidth=2                  " An autoindent is 2 spaces
set expandtab                     " Use spaces, not tabs
set list                          " Show invisible characters
set backspace=indent,eol,start    " Backspace through everything in insert mode
set autoindent                    " Always autoindent
set copyindent                    " Copy previous indentation on autoindenting
set shiftround                    " use multiple of shiftwidth when indenting
                                  "   with '<' and '>'
set smarttab                      " Insert tabs on start of line according to
                                  "   shiftwidth, not tabstop

" List chars
set listchars=""            " Reset the listchars
set listchars=tab:\ \       " a tab should display as "  ",
                            "   trailing whitespace as "."
set listchars+=trail:.      " show trailing spaces as dots
set listchars+=extends:>    " The character to show in the last column when
                            "   wrap is off and the line continues beyond the
                            "   right of the screen
set listchars+=precedes:<   " The character to show in the last column when
                            "   wrap is off and the line continues beyond the
                            "   left of the screen


" -----------------------------------------------------------------------------
" Folding
set foldmethod=indent
set foldlevelstart=99
set foldminlines=3
set foldnestmax=2

nnoremap <Space> za         " User space to toggle folds in normal mode
vnoremap <Space> za         " User space to toggle folds in visual mode
nnoremap zO zCzO            " Make zO recursively open whatever top level fold
                            "   we're in, no matter where the cursor is

" -----------------------------------------------------------------------------
" Searching

set hlsearch                " highlight matches
set incsearch               " incremental searching
set ignorecase              " searches are case insensitive...
set smartcase               " ... unless they contain at least one capital

" Keep search matches in the middle of the window and pulse the line when
" moving to them.
nnoremap n n:call PulseCursorLine()<cr>
nnoremap N N:call PulseCursorLine()<cr>


" -----------------------------------------------------------------------------
" Wild settings

set wildmenu            " make tab completion for files/buffers act like bash
set wildmode=list:full  " show a list when pressing tab and complete
                        "   first full match

" Disable output and VCS files
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.rbo,*.class,.svn,*.gem

" Disable archive files
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz

" Ignore bundler and sass cache
set wildignore+=*/vendor/gems/*,*/vendor/cache/*,*/.bundle/*,*/.sass-cache/*

" Ignore rails temporary asset caches
set wildignore+=*/tmp/cache/assets/*/sprockets/*,*/tmp/cache/assets/*/sass/*

" Disable temp and backup files
set wildignore+=*.swp,*~,._*


" -----------------------------------------------------------------------------
" Navigation

" Remap j and k to act as expected when used on long, wrapped, lines
nnoremap j gj
nnoremap k gk

" Easy window navigation (instead of ctrl-w then j it's just ctrl-j)
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>


" -----------------------------------------------------------------------------
" Key mappings

" Quickly edit/reload the vimrc file
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>    " Edit vimrc
nnoremap <silent> <leader>sv :so $MYVIMRC<CR>   " Reload vimrc

" Toggle show/hide invisible chars
nnoremap <leader>i :set list!<cr>

" Toggle line numbers
nnoremap <leader>N :setlocal number!<cr>

" Shortcut to make
nnoremap mk :make<CR>

" Quick yanking to the end of the line
nnoremap Y y$

" Yank/paste to the OS clipboard with ,y and ,p
nnoremap <leader>y "+y
nnoremap <leader>Y "+yy
nnoremap <leader>p "+p
nnoremap <leader>P "+P

" Clears the search register
nnoremap <silent> <leader>/ :nohlsearch<CR>

" Quick alignment of text
nnoremap <leader>al :left<CR>
nnoremap <leader>ar :right<CR>
nnoremap <leader>ac :center<CR>

" Sudo to write
cnoremap w!! w !sudo tee % >/dev/null

" Jump to matching pairs easily, with Tab
nnoremap <Tab> %
vnoremap <Tab> %

" Strip all trailing whitespace from a file, using ,W
nnoremap <leader>W :%s/\s\+$//<CR>:let @/=''<CR>

" Fugitive:
nmap <leader>gb :Gblame<CR>
nmap <leader>gs :Gstatus<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gl :Glog<CR>
nmap <leader>gc :Gcommit<CR>
nmap <leader>gp :Git push<CR>

" Dispatch: Mapping F9 to :Dispatch
nnoremap <F9> :Dispatch<CR>

" Tagbar: Mapping F8 to toggle Tagbar
nnoremap <F8> :TagbarToggle<CR>

" Lusty Juggler: Mapping ,b to Lusty Juggler
nnoremap <silent> <Leader>b :LustyJuggler<CR>

" Ack: Mapping ,f :Ack
nnoremap <leader>f :Ack<space>

" Quote words under cursor
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel

" Quote current selection
" TODO: This only works for selections that are created "forwardly"
vnoremap <leader>" <esc>a"<esc>gvo<esc>i"<esc>gvo<esc>ll
vnoremap <leader>' <esc>a'<esc>gvo<esc>i'<esc>gvo<esc>ll


" -----------------------------------------------------------------------------
" Plugin settings

" CtrlP
" Set CTRLP working dir to nearest ancestor that contains
" .git/, .hg/, .bzr/, _darcs/ or root.dir
let g:ctrlp_working_path_mode = 2

" Powerline
" Use compatible (non-fancy) Powerline to stop font weirdness and faffing
" about with patched fonts.
let g:Powerline_symbols = 'compatible'

" Ack
" Use the_silver_searcher program instead of ack
let g:ackprg = 'ag --nogroup --nocolor --column'

" VimWiki
" Change location of default Vimwiki to a sensible Git Annex directory
let g:vimwiki_list = [{'path': '~/annex/vimwiki'}]

" -----------------------------------------------------------------------------
" Custom functions

" Pulse

function! PulseCursorLine()
    let current_window = winnr()

    windo set nocursorline
    execute current_window . 'wincmd w'

    setlocal cursorline

    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    hi CursorLine guibg=#3a3a3a
    redraw
    sleep 20m

    hi CursorLine guibg=#4a4a4a
    redraw
    sleep 30m

    hi CursorLine guibg=#3a3a3a
    redraw
    sleep 30m

    hi CursorLine guibg=#2a2a2a
    redraw
    sleep 20m

    execute 'hi ' . old_hi

    windo set cursorline
    execute current_window . 'wincmd w'
endfunction

