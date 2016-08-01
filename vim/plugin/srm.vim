" File: srm.vim
" Author: Keith Smiley
" Description: Securely erase the current file

if !executable('srm')
  finish
endif

function! s:SecureDelete(args, bang)
  let s:file = fnamemodify(bufname(a:args), ':p')
  execute 'bdelete' . a:bang
  call system('srm ' . s:file)
  if v:shell_error != 0
    echohl ErrorMsg
    echo 'Failed to delete "' . s:file . '"'
    echohl None
  endif
  unlet s:file
endfunction

command! -bar -bang Sunlink :call s:SecureDelete(<q-args>, <q-bang>)
