" Custom alternate header/implementation files functions
function! alternate#Alternate(cmd)
  if &modified
    echoerr "Save buffer before alternating"
    return 0
  endif

  let l:fullname = expand("%")
  let l:alternate = s:SavedAlternateForFilename(l:fullname)
  if l:alternate == ""
    let l:filename = expand("%:r") . "."
    let l:extension = expand("%:e")
    let l:alternates = ["h"]
    if l:extension == "h"
      let l:alternates = ["m", "c", "cpp", "mm"]
    endif
    let l:alternate = s:AlternateFor(l:filename, l:alternates)
  endif

  if l:alternate == ""
    echomsg "No alternate file for " . l:fullname
  else
    if !exists(s:SaveStringForFilename(l:fullname))
      let l:cmd = "let " . s:SaveStringForFilename(l:fullname) . "='" . l:alternate . "'"
      execute l:cmd
    endif
    execute ":silent " . a:cmd . " " . s:EscapeFilePath(l:alternate)
  endif
endfunction

function! s:SavedAlternateForFilename(filename)
  if exists(s:SaveStringForFilename(a:filename))
    return eval(s:SaveStringForFilename(a:filename))
  endif

  return ""
endfunction

function! s:SaveStringForFilename(filename)
  return "g:alternate_for_" . s:EscapeForVar(a:filename)
endfunction

function! s:EscapeForVar(filename)
  let l:fname = a:filename
  let l:fname = substitute(l:fname, '/', '_', 'g')
  let l:fname = substitute(l:fname, '\.', '_', 'g')
  return l:fname
endfunction

function! s:EscapeFilePath(path)
  return substitute(a:path, ' ', '\\ ', 'g')
endfunction

function! s:AlternateFor(filename, extensions)
  let l:alternate = ""
  for l:extension in a:extensions
    let l:possible = a:filename . l:extension
    if filereadable(l:possible)
      return l:possible
    endif
  endfor

  return l:alternate
endfunction
