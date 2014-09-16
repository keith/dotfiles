autocmd BufNewFile,BufRead * call s:Haskell()
function! s:Haskell()
  if !empty(&filetype)
    return
  endif

  if getline(1) =~ 'runhaskell'
    setlocal filetype=haskell
  endif
endfunction
