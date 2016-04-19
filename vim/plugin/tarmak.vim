function! s:Uppercase(letter)
  return substitute(a:letter, '\(\w\)', '\u\1', '')
endfunction

function! s:Replace(first, second, ...)
  execute "noremap " . a:first . " " . a:second
  execute "xnoremap " . a:first . " " . a:second

  if a:0 == 0
    execute "noremap " . s:Uppercase(a:first) . " " . s:Uppercase(a:second)
    execute "xnoremap " . s:Uppercase(a:first) . " " . s:Uppercase(a:second)
  endif
endfunction

call s:Replace("n", "j")
call s:Replace("e", "k")
call s:Replace("k", "n")
call s:Replace("j", "e")
call s:Replace("gn", "gj", 1)
call s:Replace("gk", "gn", 1)
call s:Replace("ge", "gk", 1)

nnoremap <silent> <c-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <c-n> :TmuxNavigateDown<cr>
nnoremap <silent> <c-e> :TmuxNavigateUp<cr>
nnoremap <silent> <c-l> :TmuxNavigateRight<cr>
nnoremap <silent> <c-\> :TmuxNavigatePrevious<cr>
