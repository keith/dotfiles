function! Squash()
  normal! mi
  silent! 2,$s/^pick/squash/e
  normal! `i
endfunction

nnoremap <silent> <leader>s :call Squash()<CR>
