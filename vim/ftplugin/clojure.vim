setlocal foldmethod=marker foldmarker=(,)

let g:rbpt_colorpairs = [
      \ ['brown',       'RoyalBlue3'],
      \ ['Darkblue',    'SeaGreen3'],
      \ ['darkgray',    'DarkOrchid3'],
      \ ['darkgreen',   'firebrick3'],
      \ ['darkcyan',    'RoyalBlue3'],
      \ ['darkred',     'SeaGreen3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['brown',       'firebrick3'],
      \ ['gray',        'RoyalBlue3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['Darkblue',    'firebrick3'],
      \ ['darkgreen',   'RoyalBlue3'],
      \ ['darkcyan',    'SeaGreen3'],
      \ ['darkred',     'DarkOrchid3'],
      \ ['red',         'firebrick3'],
    \ ]


if exists(":RainbowParenthesesActivate") == 2
  autocmd Syntax * RainbowParenthesesActivate
  autocmd Syntax * RainbowParenthesesLoadRound
  autocmd Syntax * RainbowParenthesesLoadSquare
endif

" Taken from
" https://github.com/tpope/vim-sexp-mappings-for-regular-people/blob/master/plugin/sexp_mappings_for_regular_people.vim#L49-L52
nmap <buffer> >(  <Plug>(sexp_emit_head_element)
nmap <buffer> >)  <Plug>(sexp_capture_next_element)
nmap <buffer> <(  <Plug>(sexp_capture_prev_element)
nmap <buffer> <)  <Plug>(sexp_emit_tail_element)
