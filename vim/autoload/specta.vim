autocmd BufNewFile,BufRead *.m call SpectaKiwi()
function! SpectaKiwi()
  echom "here"
  let n = 1
  while n < 20 && n < line('$')
    if getline(n) =~ 'SpecBegin'
      echom "spec"
      set ft=objc
      set syntax=specta
      setlocal foldmethod=syntax
      return
    elseif getline(n) =~ 'SPEC_BEGIN'
      echom "kiwi"
      setlocal syntax=kiwi
      setlocal foldmethod=syntax
      return
    endif

    let n += 1
  endwhile
endfunction
