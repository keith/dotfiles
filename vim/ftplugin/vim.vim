setlocal foldmethod=marker
setlocal makeprg=source\ %

augroup reload_vimrc
  autocmd!
  autocmd BufWritePost *vimrc source $MYVIMRC
augroup END
