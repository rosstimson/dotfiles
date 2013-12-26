# Vim settings

## Installing

Plugins are installed and managed via
[NeoBundle](https://github.com/Shougo/neobundle.vim) which is initially
pulled down as a Git submodule, everything else in `~/.vim/bundle/` is
ignored by Git.

NeoBundle will check if plugins are installed and prompt you if they are not
when starting up Vim.  However the easiest way to get started is to run the
script in `~/.vim/bundle/neobundle.vim/bin/neoinstall`.

## Plugins

These are some of the key plugins of interest and commands / key mappings that
I often need reminded about.

* [NeoBundle](https://github.com/Shougo/neobundle.vim): Install and manage plugins.
  - `:NeoBundleList` list configured bundles
  - `:NeoBundleInstall(!)` or `:Unite neobundle/install` install(update) bundles
  - `:NeoBundleClean(!)` confirm(or auto-approve) removal of unused bundles
* [ri.vim]{https://github.com/danchoi/ri.vim}: Browse ri documentation offline in Vim.
  - `,r` open search/auto-complete window with horizontal split
  - `,R` open search/auto-complete window with vertical split
  - `,K` open search/auto-complete window and prefills with keyword under cursor
  - `K` is automatically remapped to use ri.vim if current buffer is a *.rb file
  - Inside documentation window
    - `,,r` class/module auto-completion window
    - `-` goes up from a method page into the parent class/module
    - `CTRL-o` and `CTRL-i` jump back and forth through visited pages
* [git-gutter](https://github.com/akiomik/git-gutter-vim): Shows git diff symbols
  in gutter upon saving buffer.
* [autotag.vim](https://github.com/vim-scripts/AutoTag): Whenever you save a file,
  it deletes all of its entries and invokes ctags in append mode.
* - [delimitMate](https://github.com/Raimondi/delimitMate): insert mode
  auto-completion for quotes, parens, brackets, etc.
* [undotree](https://github.com/mbbill/undotree): Visualise the undo tree.
