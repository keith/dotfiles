" Custom mappings to flash searchs as you go to them
nnoremap <silent> n   nzzzv:call HighlightNext(0.4)<CR>
nnoremap <silent> N   Nzzzv:call HighlightNext(0.4)<CR>
highlight SpecialSearchFlash guibg=DarkCyan ctermbg=DarkCyan

function! HighlightNext (blinktime)
  let target = '\c\%#' . @/
  let ring = matchadd('SpecialSearchFlash', target, 101)
  redraw
  exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
  call matchdelete(ring)
  redraw
endfunction
