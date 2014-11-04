setlocal foldmethod=marker
setlocal makeprg=source\ %
setlocal iskeyword+=:

augroup reload_vimrc
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END
