" Preamble ---------------------------------------------------------------- {{{
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

" }}}


" Bundles ----------------------------------------------------------------- {{{

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
Bundle 'bling/vim-airline'
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
Bundle 'sjl/gundo.vim'
Bundle 'scratch'
Bundle 'godlygeek/tabular'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'thoughtbot/vim-rspec'

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
Bundle 'vim-ruby/vim-ruby'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'lotheac/pf.vim'
Bundle 'nginx.vim'
Bundle 'othree/html5.vim'
Bundle 'rosstimson/bats.vim'
Bundle 'rosstimson/modx.vim'
Bundle 'php.vim'
Bundle 'remind'
Bundle 'xsbeats/vim-blade'
Bundle 'fsouza/go.vim'
Bundle 'tpope/vim-liquid'
Bundle 'Keithbsmiley/rspec.vim'

" Colour schemes
Bundle 'vim-scripts/Colour-Sampler-Pack'
Bundle 'larssmit/vim-getafe'
Bundle 'vim-scripts/molokai'
Bundle 'altercation/vim-colors-solarized'
Bundle 'tpope/vim-vividchalk'
Bundle 'twerth/ir_black'
Bundle 'sjl/badwolf'
Bundle 'chriskempson/vim-tomorrow-theme'

" }}}


" Basic settings ---------------------------------------------------------- {{{

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
set nrformats=            " Treat all numerals as decimals
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
au VimResized * :wincmd = " Resize splits when Vim is resized
set spelllang=en_gb       " Let's use real English
set spellfile=~/.vim/spell/en.utf-8.add   " Custom dictionary

set t_Co=256              " Pretty colours
set background=dark       " Setting background to dark
" TODO colorscheme ????      " Setting colourscheme

" }}}


" Whitespace -------------------------------------------------------------- {{{

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

" }}}


" Folding ----------------------------------------------------------------- {{{

set foldenable              " Enable folding
set foldcolumn=2            " Add a fold column
set foldmethod=marker       " Detect triple-{ style fold markers
set foldlevelstart=99       " Start with everything unfolded
set foldminlines=3          " Don't fold if only a few lines
set foldnestmax=2           " Don't nest fold too much

nnoremap <Space> za         " User space to toggle folds in normal mode
vnoremap <Space> za         " User space to toggle folds in visual mode
nnoremap zO zCzO            " Make zO recursively open whatever top level fold
                            "   we're in, no matter where the cursor is

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}

set foldtext=MyFoldText()

" }}}


" Searching --------------------------------------------------------------- {{{

set hlsearch                " highlight matches
set incsearch               " incremental searching
set ignorecase              " searches are case insensitive...
set smartcase               " ... unless they contain at least one capital

" Keep search matches in the middle of the window and pulse the line when
" moving to them.
nnoremap n n:call PulseCursorLine()<cr>
nnoremap N N:call PulseCursorLine()<cr>

" }}}


" Wild settings ----------------------------------------------------------- {{{

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

" }}}


" Navigation -------------------------------------------------------------- {{{

" Remap j and k to act as expected when used on long, wrapped, lines
nnoremap j gj
nnoremap k gk

" Speed up scrolling of viewport (move 3 lines at a time instead of 1)
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Easy window navigation (instead of ctrl-w then j it's just ctrl-j)
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" }}}

" Key Mappings ------------------------------------------------------------ {{{

" Quickly edit/reload the vimrc file
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>    " Edit vimrc
nnoremap <silent> <leader>sv :so $MYVIMRC<CR>   " Reload vimrc

"Remap Ctrl-C to go back to normal mode
inoremap <C-c> <Esc>

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

" Ack: Mapping ,f to :Ack for searching
nnoremap <leader>f :Ack<space>

" Gundo: Mapping F7 to toggle Gundo
nnoremap <F7> :GundoToggle<CR>

" YankRing: Mapping F3 to toggle :YRShoww
nnoremap <silent> <F3> :YRShow<cr>
inoremap <silent> <F3> <ESC>:YRShow<cr>

" Quote words under cursor
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel

" Quote current selection
vnoremap <leader>" <esc>a"<esc>gvo<esc>i"<esc>gvo<esc>ll
vnoremap <leader>' <esc>a'<esc>gvo<esc>i'<esc>gvo<esc>ll

" Tabularize: Set mappings for common usage
if exists(":Tabularize")
  nmap <Leader>a= :Tabularize /=<CR>
  vmap <Leader>a= :Tabularize /=<CR>
  nmap <Leader>a: :Tabularize /:\zs<CR>
  vmap <Leader>a: :Tabularize /:\zs<CR>
endif

" Rainbow_Parentheses: Mapping ,r to toggle colour highlighting of parentheses
nmap <leader>r :RainbowParenthesesToggle<CR>

" }}}

" Filetype specific ------------------------------------------------------- {{{

" CSS and Scss {{{
augroup ft_css
    au!

    au BufNewFile,BufRead *.scss setlocal filetype=scss

    au Filetype scss,css setlocal foldmethod=marker
    au Filetype scss,css setlocal foldmarker={,}
    au Filetype scss,css setlocal omnifunc=csscomplete#CompleteCSS
    au Filetype scss,css setlocal iskeyword+=-

    " Use <leader>S to sort properties.  Turns this:
    "
    "     p {
    "         width: 200px;
    "         height: 100px;
    "         background: red;
    "
    "         ...
    "     }
    "
    " into this:

    "     p {
    "         background: red;
    "         height: 100px;
    "         width: 200px;
    "
    "         ...
    "     }
    au BufNewFile,BufRead *.scss,*.css nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>

    " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
    " positioned inside of them AND the following code doesn't get unfolded.
    au BufNewFile,BufRead *.scss,*.css inoremap <buffer> {<cr> {}<left><cr><space><space><space><space>.<cr><esc>kA<bs>

    " Load braces for RainbowParentheses
    au BufNewFile,BufRead *.scss,*.css RainbowParenthesesLoadBraces
augroup END

" }}}

" Go {{{
augroup ft_go
    au!

    au Filetype go setlocal shiftwidth=4 tabstop=4 noexpandtab foldmethod=syntax

    " Load braces for RainbowParentheses
    au BufNewFile,BufRead *.go RainbowParenthesesLoadBraces
augroup END

" }}}

" Javascript {{{
augroup ft_javascript
    au!

    au FileType javascript setlocal foldmethod=marker
    au FileType javascript setlocal foldmarker={,}

    " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
    " positioned inside of them AND the following code doesn't get unfolded.
    au Filetype javascript inoremap <buffer> {<cr> {}<left><cr><space><space><space><space>.<cr><esc>kA<bs>
augroup END

" }}}

" Mail {{{
augroup ft_mail
    au!

    au Filetype mail setlocal spell
augroup END

" }}}

" Markdown {{{
augroup ft_markdown
    au!

    au BufNewFile,BufRead *.m*down setlocal filetype=markdown
    au FileType markdown setlocal foldlevel=1
    au FileType markdown setlocal spell

    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=:redraw<cr>
    au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-:redraw<cr>
    au Filetype markdown nnoremap <buffer> <localleader>3 mzI###<space><ESC>
augroup END

" }}}

" Mutt {{{
augroup ft_muttrc
    au!

    au BufRead,BufNewFile *.muttrc set ft=muttrc

    au FileType muttrc setlocal foldmethod=marker foldmarker={{{,}}}
augroup END

" }}}

" Nginx {{{
augroup ft_nginx
    au!

    au BufRead,BufNewFile /etc/nginx/conf/*                      set ft=nginx
    au BufRead,BufNewFile /etc/nginx/sites-available/*           set ft=nginx
    au BufRead,BufNewFile /usr/local/etc/nginx/sites-available/* set ft=nginx
    au BufRead,BufNewFile vhost.nginx                            set ft=nginx

    au FileType nginx setlocal tabstop=4 shiftwidth=4 foldmethod=marker foldmarker={,}
augroup END

" }}}

" PF {{{
augroup ft_pf
    au!

    au BufRead,BufNewFile /etc/pf.conf        set ft=pf
augroup END

" }}}

" PHP {{{
augroup ft_php
    au!

    au Filetype php setlocal shiftwidth=4 softtabstop=4 tabstop=4 foldmethod=syntax

    " Load braces for RainbowParentheses
    au BufNewFile,BufRead *.scss,*.css RainbowParenthesesLoadBraces
augroup END

" }}}

" Ruby {{{
augroup ft_ruby
    au!
    au Filetype ruby setlocal foldmethod=syntax
augroup END

" }}}

" }}}

" Plugin settings / options ----------------------------------------------- {{{

" CtrlP
" Set CTRLP working dir to nearest ancestor that contains
" .git/, .hg/, .bzr/, _darcs/ or root.dir
let g:ctrlp_working_path_mode = 2

" Vim-Airline
" TODO Use fancy symbols without patched font, certain chars aren't working 
" when using the new Powerline recommended fontconfig way.
" let g:airline_powerline_fonts = 1

" Ack
" Use the_silver_searcher program instead of ack
let g:ackprg = 'ag --nogroup --nocolor --column'

" VimWiki
" Change location of default Vimwiki to a sensible Git Annex directory
let g:vimwiki_list = [{'path': '$HOME/annex/vimwiki'}]

" Dispatch
" Run current file through Ruby interpreter and check syntax
autocmd FileType ruby let b:dispatch = 'ruby -wc %'

" UltiSnips
" Define custom snippets directory
let g:UltiSnipsSnippetsDir = "$HOME/.vim/snippets"

" Rspec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>
" Support custom commands / test runners
" let g:rspec_command = "Dispatch zeus rspec {spec}"


" }}}


" Custom functions -------------------------------------------------------- {{{

" Pulse {{{
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

" }}}


" Scratch {{{
command! ScratchToggle call ScratchToggle()

function! ScratchToggle()
    if exists("w:is_scratch_window")
        unlet w:is_scratch_window
        exec "q"
    else
        exec "normal! :ScratchOpen\<cr>\<C-W>K"
        let w:is_scratch_window = 1
    endif
endfunction

nnoremap <silent> <leader><tab> :ScratchToggle<cr>

" }}}

" }}}
