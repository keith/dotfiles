" Detect htmldjango files
autocmd BufNewFile,BufRead *.html call DjangoHTML()
function! DjangoHTML()
  " Don't override already set non-html filetypes
  if !empty(&filetype) && &filetype !=# "html"
    return
  endif

  let n = 1
  while n < 5 && n < line('$')
    if getline(n) =~ '{%\|{{\|{#'
      setlocal filetype=htmldjango
      return
    endif

    let n += 1
  endwhile

  setfiletype html
endfunction
