" Custom alternate header/implementation files functions ------ {{{
function! Alternate(cmd)
  if &modified
    echoerr "Save buffer before alternating"
    return 0
  endif

  let l:filename = expand("%:r") . "."
  let l:extension = expand("%:e")
  let l:alternates = ["h"]
  if l:extension == "h"
    let l:alternates = ["m", "c", "cpp", "mm"]
  endif
  let l:alternate = AlternateFor(l:filename, l:alternates)
  if l:alternate == ""
    echomsg "No alternate file for " . expand("%")
  else
    execute ":silent " . a:cmd . " " . EscapeFilePath(l:alternate)
  endif
endfunction

function! AlternateFor(filename, extensions)
  let l:alternate = ""
  for l:extension in a:extensions
    let l:possible = a:filename . l:extension
    if filereadable(l:possible)
      return l:possible
    endif
  endfor

  return l:alternate
endfunction

augroup alternate_headers
  autocmd!
  autocmd FileType objc,c,cpp command! -buffer A :call Alternate("edit")
  autocmd FileType objc,c,cpp command! -buffer AS :call Alternate("split")
  autocmd FileType objc,c,cpp command! -buffer AV :call Alternate("vsplit")
  autocmd FileType objc,c,cpp command! -buffer AT :call Alternate("tabnew")
augroup END
" }}}
