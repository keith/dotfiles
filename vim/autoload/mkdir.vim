function! mkdir#Mkdir()
  let dir = expand("%:p:h")
  if !isdirectory(dir)
    call mkdir(dir, 'p')
    echo "Creating: " . dir
  endif
endfunction
