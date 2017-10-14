" Run swift-demangle on the word under the cursor
nnoremap <silent> <leader>d :call <SID>SwiftDemangle(expand('<cword>'))<CR>
function! s:SwiftDemangle(word)
  let l:output = system('xcrun swift-demangle ' . a:word)
  if v:shell_error
    echohl ErrorMsg
    echom "Failed to demangle"
    echohl None
    return
  endif

  echom substitute(l:output, '\n$', '', '')
endfunction
