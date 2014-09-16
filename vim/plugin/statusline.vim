function! ModifiedString()
  if &modified 
    return ' [+]'
  else 
    return ''
  endif
endfunction

let g:currentmode = {
  \ '' : 'S-Block',
  \ '' : 'V-Block',
  \ '!'  : 'Shell',
  \ 'c'  : 'Command',
  \ 'ce' : 'Ex',
  \ 'cv' : 'Vim Ex',
  \ 'i'  : 'Insert',
  \ 'n'  : 'Normal',
  \ 'no' : 'N-Operator Pending',
  \ 'r'  : 'Prompt',
  \ 'R'  : 'Replace',
  \ 'r?' : 'Confirm',
  \ 'rm' : 'More',
  \ 'Rv' : 'V-Replace',
  \ 'S'  : 'S-Line',
  \ 's'  : 'Select',
  \ 'V'  : 'V-Line',
  \ 'v'  : 'Visual',
\ }

function! PasteString()
  if &paste
    return ' > PASTE'
  else
    return ''
  endif
endfunction

function! ModeString(mode)
  if a:mode == "i"
    hi User1 ctermbg=1
  " elseif a:mode == "n"
  else
    hi User1 ctermbg=2
  endif
  return '  ' . toupper(g:currentmode[a:mode]) . PasteString() . ' '
endfunction

" Status line setup (without plugins)
" Left Side
set statusline=
set statusline+=%1*%{ModeString(mode())}%*
set statusline+=\ %f
set statusline+=%{ModifiedString()}
set statusline+=\ %r
" Right Side
set statusline+=%=
set statusline+=%{&ft}
set statusline+=\ \ %P
set statusline+=\ :\ %l:
set statusline+=\ %c\ \ |
