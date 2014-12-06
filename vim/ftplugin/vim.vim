setlocal foldmethod=marker
setlocal keywordprg=:help
setlocal makeprg=source\ %

augroup reload_vimrc
  autocmd!
  autocmd BufWritePost *vimrc source $MYVIMRC
augroup END
