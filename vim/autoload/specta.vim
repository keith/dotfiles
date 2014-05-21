function! specta#TestSyntaxSetup()
  let n = 1
  while n < 15 && n < line('$')
    if getline(n) =~ 'SpecBegin'
      setlocal syntax=specta
      setlocal foldmethod=syntax
      return
    elseif getline(n) =~ 'SPEC_BEGIN'
      setlocal syntax=kiwi
      setlocal foldmethod=syntax
      return
    endif

    let n += 1
  endwhile
endfunction
