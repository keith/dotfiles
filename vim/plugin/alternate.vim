" Custom alternate header/implementation files functions ------ {{{
function! Alternate(cmd)
  if &modified
    echoerr "Save buffer before alternating"
    return 0
  endif

  let l:fullname = expand("%")
  let l:alternate = SavedAlternateForFilename(l:fullname)
  if l:alternate == ""
    let l:filename = expand("%:r") . "."
    let l:extension = expand("%:e")
    let l:alternates = ["h"]
    if l:extension == "h"
      let l:alternates = ["m", "c", "cpp", "mm"]
    endif
    let l:alternate = AlternateFor(l:filename, l:alternates)
  endif

  if l:alternate == ""
    echomsg "No alternate file for " . l:fullname
  else
    if !exists(SaveStringForFilename(l:fullname))
      let l:cmd = "let " . SaveStringForFilename(l:fullname) . "='" . l:alternate . "'"
      execute l:cmd
    endif
    execute ":silent " . a:cmd . " " . EscapeFilePath(l:alternate)
  endif
endfunction

function! SavedAlternateForFilename(filename)
  if exists(SaveStringForFilename(a:filename))
    return eval(SaveStringForFilename(a:filename))
  endif

  return ""
endfunction

function! SaveStringForFilename(filename)
  return "g:alternate_for_" . EscapeForVar(a:filename)
endfunction

function! EscapeForVar(filename)
  let l:fname = a:filename
  let l:fname = substitute(l:fname, '/', '_', 'g')
  let l:fname = substitute(l:fname, '\.', '_', 'g')
  return l:fname
endfunction

function! EscapeFilePath(path)
  return substitute(a:path, ' ', '\\ ', 'g')
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
