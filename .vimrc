" ~/.vimrc

" minpac package manager (https://github.com/k-takata/minpac)
packadd minpac

call minpac#init()

" minpac must have {'type': 'opt'} so that it can be loaded with `packadd`.
call minpac#add('k-takata/minpac', {'type': 'opt'})

" Add other plugins here.
call minpac#add('NLKNguyen/papercolor-theme')
call minpac#add('hashivim/vim-terraform')
call minpac#add('lotabout/skim', { 'do': 'silent !./install' })
call minpac#add('lotabout/skim.vim')
call minpac#add('mileszs/ack.vim')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-git')
call minpac#add('tpope/vim-surround')


" Use Vim settingr, rather than Vi settings.
set nocompatible

" Set colour scheme.
set termguicolors     " enable true colors support
set background=light
colorscheme PaperColor

set history=500       " Keep 500 lines of command line history.
set ruler             " Show the cursor position all the time.
set showcmd           " Display incomplete commands.
set wildmenu          " Display completion matches in a status line.
set ttimeout          " Time out for key codes.
set ttimeoutlen=100   " Wait up to 100ms after Esc for special key.

" Set UTF-8 as default encoding
set encoding=utf-8

" Enable filetype detection.
filetype plugin indent on

" Switch syntax highlighting on when the terminal has colors or when using the
" GUI (which always has colors).
if &t_Co > 2 || has("gui_running")
  " Revert with ":syntax off".
  syntax on
endif

" Show @@@ in the last line if it is truncated.
set display=truncate

" Show a few lines of context around the cursor.  Note that this makes the
" text scroll if you mouse-click near the start or end of the window.
set scrolloff=5

" Do incremental searching when it's possible to timeout.
if has('reltime')
  set incsearch
endif

" Show matching parentheses
set showmatch

" Case insensitive search unless search term has capital letters.
set ignorecase
set smartcase

" Don't keep backup files, it's 70s style clutter.
set nobackup

" Don't write annoying intermediate swap files, who ever restored from swap
" files anyway.
set noswapfile

" Open horizontal splits at the bottom as this feels more natural.
set splitbelow

" Open vertical splits at the right as this feels more natural.
set splitright

" Keep 5 lines off the edges of the screen when scrolling.
set scrolloff=5

" Set spelling options
set spelllang=en_gb
set spellfile=~/.vim/spell/en.utf-8.add

" rg -- Make :Ack use ripgrep when available.
if executable('rg')
  let g:ackprg = 'rg --vimgrep'
endif

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" Highlight all search results
set hlsearch

" Toggle the search highlighting
noremap <leader>/ :set hlsearch! hlsearch?<CR>

" Toggle show/hide invisible chars
nnoremap <leader>w :set list!<cr>

" Whitespace
set nowrap                          " Don't wrap lines
set tabstop=2                       " A tab is 2 spaces
set shiftwidth=2                    " An autoindent is 2 spaces
set expandtab                       " Use spaces, not tabs
set list                            " Show invisible characters
set backspace=indent,eol,start      " Backspace through everything in insert mode
set autoindent                      " Always autoindent

" List chars / highlight whitespace
set listchars=eol:$,tab:›\ ,trail:~,nbsp:.,extends:>,precedes:<


" Misc Keybindings

" Insert mode navigations
inoremap <C-k> <Up>
inoremap <C-j> <Down>
inoremap <C-h> <Left>
inoremap <C-l> <Right>

" Insert literal tab
inoremap <S-Tab> <C-V><Tab>

" Toggle line numbers
nnoremap <leader>n :set number!<CR>
