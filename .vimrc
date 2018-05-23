" Preamble ---------------------------------------------------------------- {{{
"
" ~/.vimrc
" My Vim preference
" Maintained by Ross Timson <ross@rosstimson.com>
"
" }}}

" Bundles ----------------------------------------------------------------- {{{

set nocompatible  " be iMproved (MUST be first)

" Use vim-plug (https://github.com/junegunn/vim-plug) to manage plugins.
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Tools
Plug 'tpope/vim-git'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-eunuch'
Plug 'mileszs/ack.vim'
Plug 'Shougo/neosnippet.vim' | Plug 'Shougo/neosnippet-snippets'
Plug 'itchyny/lightline.vim'
Plug 'majutsushi/tagbar'
Plug 'godlygeek/tabular'
Plug 'akiomik/git-gutter-vim'
Plug 'mbbill/undotree'
Plug 'benekastah/neomake'
Plug 'tweekmonster/braceless.vim'

" Langs
Plug 'plasticboy/vim-markdown'
Plug 'fatih/vim-go'
Plug 'stephpy/vim-yaml'
Plug 'saltstack/salt-vim'
Plug 'othree/html5.vim'
Plug 'klen/python-mode'

" Colour schemes
Plug 'vim-scripts/Colour-Sampler-Pack'
Plug 'chriskempson/base16-vim'
Plug 'larssmit/vim-getafe'
Plug 'vim-scripts/molokai'
Plug 'altercation/vim-colors-solarized'
Plug 'tpope/vim-vividchalk'
Plug 'twerth/ir_black'
Plug 'sjl/badwolf'
Plug 'chriskempson/base16-vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'junegunn/seoul256.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'morhetz/gruvbox'
Plug 'whatyouhide/vim-gotham'

" End vim-plug shenanigans
call plug#end()

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
set undofile              " Persistent undos
set undodir=$HOME/.vim/undo " Where to save undo histories
set undolevels=1000       " How many undos
set undoreload=10000      " Number of lines to save for undo
set viminfo='20,\"80      " Read / Write a .viminfo file, don't store more
                          "   than 80 lines of registers
set title                 " Change the terminal's title
set visualbell            " Don't beep
set t_vb=                 " Don't flash
set noerrorbells          " Don't beep
set showcmd               " Show (partial) command in the last line of the
                          "   screen this also shows visual selection info
set modeline              " Enable modeline
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
set completeopt-=preview  " Disable scratch/preview split window for omnicompletions
set t_Co=256              " Pretty colours
let base16colorspace=256  " Access colors present in 256 colorspace
let g:seoul256_background = 234 " Darker background when using seoul256 theme
set background=dark       " Setting background to dark

" Setting default colourscheme
silent! colorscheme jellybeans

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
set listchars=tab:›\ ,trail:.,nbsp:. " Highlight problematic whitespace
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

" Ignore output and VCS files
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.rbo,*.class,.svn,*.gem

" Ignore archive files
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.tgz,*.txz

" Ignore bundler and sass cache
set wildignore+=*/vendor/gems/*,*/vendor/cache/*,*/.bundle/*,*/.sass-cache/*

" Ignore rails temporary asset caches
set wildignore+=*/tmp/cache/assets/*/sprockets/*,*/tmp/cache/assets/*/sass/*

" Ignore temp and backup files
set wildignore+=*.swp,*~,._*

" }}}


" Navigation -------------------------------------------------------------- {{{

" Remap j and k to act as expected when used on long, wrapped, lines
nnoremap j gj
nnoremap k gk

" Speed up scrolling of viewport (move 3 lines at a time instead of 1)
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Easier horizontal scrolling
map zl zL
map zh zH

" Easy buffer and tab switching
nnoremap <C-J> :bnext<CR>
nnoremap <C-K> :bprev<CR>
nnoremap <C-L> :tabn<CR>
nnoremap <C-H> :tabp<CR>

" Use - for opening NetRW at dir of current file (like Filebeagle did).
nnoremap - :Explore<CR>

" }}}

" Key Mappings ------------------------------------------------------------ {{{

" Quickly edit/reload the vimrc file
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>    " Edit vimrc
nnoremap <silent> <leader>sv :so $MYVIMRC<CR>   " Reload vimrc

" Toggle show/hide invisible chars
nnoremap <leader>i :set list!<cr>

" Toggle line numbers
nnoremap <leader>N :setlocal number!<cr>

" Fast window resizing
" Vertical resizing with + / -
" Horizontal resizing with Ctrl-n / Ctrl-m
if bufwinnr(1)
  map + :resize +5<CR>
  map - :resize -5<CR>
  map <C-N> :vertical resize +5<CR>
  map <C-M> :vertical resize -5<CR>
endif

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

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" Allow using the repeat operator with a visual selection (!)
vnoremap . :normal .<CR>

" NeoVim Terminal
" Exit terminal with ,e
if has('nvim')
  tnoremap <Leader>e <C-\><C-n>
endif

" Fugitive:
nmap <leader>gb :Gblame<CR>
nmap <leader>gs :Gstatus<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gls :Glog<CR>
nmap <leader>gc :Gcommit<CR>
nmap <leader>gp :Git push<CR>
nmap <leader>gl :Git pull<CR>

" Tagbar: Mapping F8 to toggle Tagbar
nnoremap <F8> :TagbarToggle<CR>

" Ack: Mapping ,a to :Ack for searching
nnoremap <leader>a :Ack<space>

" Undotree: Mapping ,ut to Undotree
nnoremap <leader>ut :UndotreeToggle<CR>

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

" Neosnippet:
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" UTL:
" Use/Goto linked thing
nmap <C-c> :Utl ol<cr>

" }}}

" Filetype specific ------------------------------------------------------- {{{

" C {{{
augroup ft_c
    au!

    " Ignore indents caused by parentheses in OpenBSD style.
    function! IgnoreParenIndent()
        let indent = cindent(v:lnum)

        if indent > 4000
            if cindent(v:lnum - 1) > 4000
                return indent(v:lnum - 1)
            else
                return indent(v:lnum - 1) + 4
            endif
        else
            return (indent)
        endif
    endfun


    " Follow the OpenBSD style(9).
    au filetype c setlocal cindent
    au filetype c setlocal cinoptions=(4200,u4200,+0.5s,*500,:0,t0,U4200
    au filetype c setlocal indentexpr=IgnoreParenIndent()
    au filetype c setlocal indentkeys=0{,0},0),:,0#,!^F,o,O,e
    au filetype c setlocal noexpandtab
    au filetype c setlocal shiftwidth=8
    au filetype c setlocal tabstop=8
    au filetype c setlocal textwidth=80
augroup END

" }}}

" CSS and Scss {{{
augroup ft_css
    au!

    au BufNewFile,BufRead *.scss setlocal filetype=scss

    au filetype scss,css setlocal foldmethod=marker
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
augroup END

" }}}

" Go {{{
augroup ft_go
    au!

    au Filetype go setlocal shiftwidth=4 tabstop=4 noexpandtab foldmethod=syntax

    let g:go_fmt_command = "goimports"
augroup END

" }}}

" HTML {{{
augroup ft_html
    au!

    au FileType html setlocal omnifunc=htmlcomplete#CompleteTags
augroup END

" }}}

" Javascript {{{
augroup ft_javascript
    au!

    au FileType javascript setlocal foldmethod=marker
    au FileType javascript setlocal foldmarker={,}
    au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS

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
    au FileType markdown setlocal spell
    au FileType markdown setlocal omnifunc=htmlcomplete#CompleteTags

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

" Outliner {{{
augroup ft_votl
    au!

    au BufNewFile,BufRead *.otl,*.votl setlocal filetype=votl
    au FileType votl setlocal spell
    au FileType votl setlocal foldminlines=0 foldnestmax=20

" }}}

" Python {{{
augroup ft_python
    au!

    au Filetype python setlocal shiftwidth=4 softtabstop=4 tabstop=4
    au FileType python setlocal foldmethod=indent foldlevel=2 foldnestmax=4
    au FileType python setlocal omnifunc=pythoncomplete#Complete

    au FileType python BracelessEnable +indent +fold

    " Jedi
    let g:jedi#popup_on_dot = 0 " Don't automatically start completion if you type a dot.
    let g:jedi#goto_assignments_command = "<leader>g"
    let g:jedi#goto_definitions_command = "<leader>d"
    let g:jedi#documentation_command = "K"
    let g:jedi#usages_command = "<leader>n"
    let g:jedi#completions_command = "<C-Space>"
    let g:jedi#rename_command = "<leader>r"
    let g:jedi#show_call_signatures = "1"

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


" rg -- Make :Ack use ripgrep when available.
if executable('rg')
  let g:ackprg = 'rg --vimgrep'
endif

" Lightline
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'fugitive', 'filename' ] ]
      \ },
      \ 'component_function': {
      \   'fugitive': 'LightLineFugitive',
      \   'readonly': 'LightLineReadonly',
      \   'modified': 'LightLineModified',
      \   'filename': 'LightLineFilename'
      \ }
      \ }


" Neosnippet
" Set if you want snippets other than those provided by neosnippet-snippets.
" let g:neosnippet#snippets_directory='~/.vim/snippets'

" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif


" Tagbar (gotags)
let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'
\ }

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

" Lightline {{{
function! LightLineModified()
  if &filetype == "help"
    return ""
  elseif &modified
    return "+"
  elseif &modifiable
    return ""
  else
    return ""
  endif
endfunction

function! LightLineReadonly()
  if &filetype == "help"
    return ""
  elseif &readonly
    return "RO"
  else
    return ""
  endif
endfunction

function! LightLineFugitive()
  return exists('*fugitive#head') ? fugitive#head() : ''
endfunction

function! LightLineFilename()
  return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
       \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
       \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
endfunction

" }}}

" fzy {{{
if has('nvim')
  " This function and keybinding needs to differ slightly between Neovim
  " and standard Vim.
  function! FzyCommand(choice_command, vim_command) abort
      let l:callback = {
                  \ 'window_id': win_getid(),
                  \ 'filename': tempname(),
                  \  'vim_command':  a:vim_command
                  \ }

      function! l:callback.on_exit(job_id, data, event) abort
          bdelete!
          call win_gotoid(self.window_id)
          if filereadable(self.filename)
              try
                  let l:selected_filename = readfile(self.filename)[0]
                  exec self.vim_command . l:selected_filename
              catch /E684/
              endtry
          endif
          call delete(self.filename)
      endfunction

      botright 10 new
      let l:term_command = a:choice_command . ' | fzy > ' .  l:callback.filename
      silent call termopen(l:term_command, l:callback)
      setlocal nonumber norelativenumber
      startinsert
  endfunction

  nnoremap <silent> <c-p> :call FzyCommand('rg --files .', ':e ')<cr>
else
  " Fallback function and keybinding for standard Vim.
  function! FzyCommand(choice_command, vim_command)
    try
      let output = system(a:choice_command . " | fzy ")
    catch /Vim:Interrupt/
      " Swallow errors from ^C, allow redraw! below
    endtry
    redraw!
    if v:shell_error == 0 && !empty(output)
      exec a:vim_command . ' ' . output
    endif
  endfunction

  nnoremap <c-p> :call FzyCommand("rg --files .", ":e")<cr>
endif

" }}}

" }}}
