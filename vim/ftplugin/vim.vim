setlocal foldmethod=marker
setlocal makeprg=source\ %

augroup reload_vimrc
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END
