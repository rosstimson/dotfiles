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
"                 --with-x \
"                 --enable-gui=yes \
"                 --enable-perlinterp \
"                 --enable-pythoninterp \
"                 --enable-rubyinterp \
"                 --enable-xim \
"                 --enable-multibyte \
"                 --with-tlib=ncurses \
"                 --enable-fontset \
"                 --enable-cscope \
"                 --disable-tclinterp \
"                 --disable-netbeans \
"                 --with-compiledby="Ross Timson <ross@rosstimson.com>"
"
" To start vim without using this .vimrc file, use:
"     vim -u NORC
"
" To start vim without loading any .vimrc or plugins, use:
"     vim -u NONE
"

if  has('vim_starting')
  set nocompatible  " be iMproved (MUST be first)
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Use NeoBundle for installing / managing Vim scripts
call neobundle#rc(expand('~/.vim/bundle/'))

" }}}


" Bundles ----------------------------------------------------------------- {{{

" Let NeoBundle manage NeoBundle (ooh how meta)
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/vimproc', {
  \ 'build' : {
  \     'windows' : 'make -f make_mingw32.mak',
  \     'cygwin' : 'make -f make_cygwin.mak',
  \     'mac' : 'make -f make_mac.mak',
  \     'unix' : 'make -f make_unix.mak',
  \    },
  \ }

" Tools
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-jdaddy'
NeoBundle 'mileszs/ack.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'bling/vim-airline'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'mattn/gist-vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'ZoomWin'
NeoBundle 'rstacruz/sparkup', {'rtp': 'vim/'}
NeoBundle 'gnupg'
NeoBundle 'nelstrom/vim-markdown-folding'
NeoBundle 'joonty/vim-phpqa'
NeoBundle 'joonty/vdebug'
NeoBundle 'scratch'
NeoBundle 'godlygeek/tabular'
NeoBundle 'kien/rainbow_parentheses.vim'
NeoBundle 'thoughtbot/vim-rspec'
NeoBundle 'danchoi/ri.vim'
NeoBundle 'akiomik/git-gutter-vim'
NeoBundle 'vim-scripts/AutoTag'
NeoBundle 'Raimondi/delimitMate'
NeoBundle 'mbbill/undotree'
NeoBundle 'xolox/vim-notes'
NeoBundle 'xolox/vim-misc'
NeoBundle 'xolox/vim-shell'
NeoBundle 'vim-scripts/VOoM'

" Langs
NeoBundle 'tpope/vim-git'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'tpope/vim-git'
NeoBundle 'nono/vim-handlebars'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'sunaku/vim-ruby-minitest'
NeoBundle 'tpope/vim-rails'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'cakebaker/scss-syntax.vim'
NeoBundle 'lotheac/pf.vim'
NeoBundle 'nginx.vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'rosstimson/bats.vim'
NeoBundle 'rosstimson/modx.vim'
NeoBundle 'php.vim'
NeoBundle 'remind'
NeoBundle 'xsbeats/vim-blade'
NeoBundle 'fatih/vim-go'
NeoBundle 'tpope/vim-liquid'
NeoBundle 'Keithbsmiley/rspec.vim'
NeoBundle 'slim-template/vim-slim'
NeoBundle 'elixir-lang/vim-elixir'

" Colour schemes
NeoBundle 'vim-scripts/Colour-Sampler-Pack'
NeoBundle 'larssmit/vim-getafe'
NeoBundle 'vim-scripts/molokai'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'tpope/vim-vividchalk'
NeoBundle 'twerth/ir_black'
NeoBundle 'sjl/badwolf'
NeoBundle 'chriskempson/vim-tomorrow-theme'

" }}}


" Basic settings ---------------------------------------------------------- {{{

filetype plugin indent on " Required! Enable detection, plugins and indenting
NeoBundleCheck            " Installation check
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
set undofile              " Persistent undos
set undodir=$HOME/.vim/undo " Where to save undo histories
set undolevels=1000       " How many undos
set undoreload=10000      " Number of lines to save for undo
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
set omnifunc=syntaxcomplete#Complete

set t_Co=256              " Pretty colours
set background=dark       " Setting background to dark

" Setting default colourscheme for terminal vim.  For this to look good it
" should match the Xorg colours specified in ~/.Xresources and ~/.Xcolors
colorscheme Tomorrow-Night-Eighties

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

" Ack: Mapping ,a to :Ack for searching (actually uses the_silver_searcher)
nnoremap <leader>a :Ack<space>

" Undotree: Mapping ,ut to Undotree
nnoremap <leader>ut :UndotreeToggle<CR>

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
  nmap <Leader>t= :Tabularize /=<CR>
  vmap <Leader>t= :Tabularize /=<CR>
  nmap <Leader>t: :Tabularize /:\zs<CR>
  vmap <Leader>t: :Tabularize /:\zs<CR>
endif

" Rainbow_Parentheses: Mapping ,r to toggle colour highlighting of parentheses
nmap <leader>rp :RainbowParenthesesToggle<CR>

" Rspec.vim mappings
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>sf :call RunCurrentSpecFile()<CR>
map <Leader>sl :call RunLastSpec()<CR>
map <Leader>sa :call RunAllSpecs()<CR>

" Unite.vim mappings
nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files -start-insert file<cr>
nnoremap <leader>fr :<C-u>Unite -no-split -buffer-name=files -start-insert file_rec/async<cr>
nnoremap <leader>ft :<C-u>Unite -no-split -buffer-name=files -start-insert -default-action=tabopen file_rec/async<cr>
nnoremap <leader>fs :<C-u>Unite -no-split -buffer-name=files -start-insert -default-action=split file_rec/async<cr>
nnoremap <leader>fv :<C-u>Unite -no-split -buffer-name=files -start-insert -default-action=vsplit file_rec/async<cr>
nnoremap <leader>b :<C-u>Unite -no-split -buffer-name=buffers -quick-match buffer<cr>
nnoremap <leader>o :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
nnoremap <leader>ru :<C-u>Unite -no-split -buffer-name=mru -start-insert file_mru<cr>
nnoremap <leader>yh :<C-u>Unite -no-split -buffer-name=yank_history history/yank<cr>

" Notes
nnoremap <leader>n :Note! collect<CR>

" vim-shell
:let g:shell_mappings_enabled = 0 " Disable default mappings
:inoremap <Leader>fs <C-o>:Fullscreen<CR>
:nnoremap <Leader>fs :Fullscreen<CR>
:inoremap <Leader>op <C-o>:Open<CR>
:nnoremap <Leader>op :Open<CR>


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

    " Load braces for RainbowParentheses
    au BufNewFile,BufRead *.js,*.json RainbowParenthesesLoadBraces
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

" Vim {{{
au FileType vim let b:loaded_delimitMate = 0 " no autoclose brackets for Vim files

" }}}

" }}}

" Plugin settings / options ----------------------------------------------- {{{

" Vim-Airline
" TODO Use fancy symbols without patched font, certain chars aren't working
" when using the new Powerline recommended fontconfig way.
" let g:airline_powerline_fonts = 1

" Ack
" Use the_silver_searcher program instead of ack
let g:ackprg = 'ag --nogroup --nocolor --column'

" Dispatch
" Run current file through Ruby interpreter and check syntax
autocmd FileType ruby let b:dispatch = 'ruby -wc %'
autocmd FileType go let b:dispatch = 'go build %'

" UltiSnips
" Define custom snippets directory
let g:UltiSnipsSnippetsDir = "$HOME/.vim/snippets"

" Rspec.vim
" Support custom commands / test runners
" let g:rspec_command = "Dispatch zeus rspec {spec}"

" Unite.vim
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])

autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction

" Notes
let g:notes_directories = ['~/Annex/Notes'] " Default dir for notes
let g:notes_suffix = '.md' " Default to markdown files
let g:notes_title_sync = 'rename_file' " Rename file on disk to match title
let g:notes_smart_quotes = 0 " Don't use special symbols

" Gist
let g:gist_open_browser_after_post = 1
let g:gist_browser_command = 'firefox %URL% &'

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
