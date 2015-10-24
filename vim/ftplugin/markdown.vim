setlocal omnifunc=htmlcomplete#CompleteTags
setlocal spell
setlocal textwidth=72

function! s:Marked()
  silent !mark %
  redraw!
endfunction

function! s:Markdown()
  silent !open -a MacDown %
  redraw!
endfunction

if has("mac")
  command! -buffer Marked call s:Marked()
  command! -buffer MD call s:Markdown()
endif
