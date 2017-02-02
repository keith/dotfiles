setlocal foldmethod=marker
setlocal keywordprg=:help

augroup reload_vimrc
  autocmd!
  autocmd BufWritePost *vimrc source $MYVIMRC
augroup END
