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

" Clears the search register
nnoremap <silent> <leader>/ :nohlsearch<CR>

" Toggle show/hide invisible chars
nnoremap <leader>w :set list!<cr>

