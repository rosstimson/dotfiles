" ~/.vimrc

" minpac package manager (https://github.com/k-takata/minpac)
packadd minpac

call minpac#init()

" minpac must have {'type': 'opt'} so that it can be loaded with `packadd`.
call minpac#add('k-takata/minpac', {'type': 'opt'})

" Add other plugins here.
call minpac#add('tpope/vim-git')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-commentary')
call minpac#add('mileszs/ack.vim')

" Load some sensible defaults.
source $VIMRUNTIME/defaults.vim

" Set UTF-8 as default encoding
set encoding=utf-8

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
