function! s:Mkdir(path)
  let dir = expand(a:path . ':p:h')
  if !isdirectory(dir)
    call mkdir(dir, 'p')
    echo "Creating: " . dir
  endif
endfunction

augroup mkdir
  autocmd!
  autocmd BufWritePre * call s:Mkdir('<afile>')
augroup END
