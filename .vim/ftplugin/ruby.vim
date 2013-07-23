" Set make program to allow easy syntax checking by running the current
" file through :make. The vim-ruby default is to hang and wait for stdin
" as it wants you to pass it a filename to run.
compiler ruby
setlocal makeprg=ruby\ -wc\ %
