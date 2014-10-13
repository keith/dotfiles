function! Rebase()
  normal! mi
  2,$s/^pick/squash/
  let @/=""
  normal! `i
endfunction

nnoremap <silent> <leader>r :call Rebase()<CR>
