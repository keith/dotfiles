setlocal omnifunc=htmlcomplete#CompleteTags
setlocal spell
setlocal textwidth=72

function! Marked()
  if !executable("mark")
    echohl ErrorMsg
    echom "Missing 'mark' CLI"
    echohl None
    return
  endif

  silent !mark %
  redraw!
endfunction

if has("mac")
  command! -buffer -bang Marked :call Marked()
endif
