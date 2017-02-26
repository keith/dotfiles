function! <SID>OpenMuttManual()
  let l:string = substitute(expand('<cword>'), '_', '-', 'g')
  execute system('open http://www.mutt.org/doc/manual/#' . l:string)
endfunction

nnoremap K :call <SID>OpenMuttManual()<CR>
