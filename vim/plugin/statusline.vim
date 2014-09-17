function! ModifiedString()
  if &modified 
    return ' [+]'
  else 
    return ''
  endif
endfunction

function! s:CheckMixedIndent()
  let t_s_t = '(^\t* +\t\s*\S)'
  let t_l_s = '(^\t+ {' . &ts . ',}' . '\S)'
  return search('\v' . t_s_t . '|' . t_l_s, 'nw')
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

function! s:ColorForGroup(group)
  return synIDattr(hlID(a:group), "fg#")
endfunction

function! ModeString(mode)
  let type = "StatusLine"
  if a:mode == "i"
    let type = "PreProc"
  elseif a:mode == "v" || a:mode == ""
    let type = "Todo"
  endif
  let color = s:ColorForGroup(type)
  execute 'hi User1 cterm=bold ctermbg=' . color
  return '  ' . toupper(g:currentmode[a:mode]) . PasteString() . ' '
endfunction

function! Spacing()
  let trailing = search('\s$', 'nw')
  let mixed = s:CheckMixedIndent()
  let output = ''
  if trailing
    let output = '  ' . printf('trailing[%s]', trailing) . ' '
  elseif mixed
    let output = '  ' . printf('mixed-indent[%s]', trailing) . ' '
  endif

  return output
endfunction

execute 'hi User2 cterm=bold ctermbg=' . s:ColorForGroup('StatusLine')
execute 'hi User3 cterm=bold ctermbg=' . s:ColorForGroup('PreProc')

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
set statusline+=%2*\ \ \ %P
set statusline+=\ :\ %l:
set statusline+=\ %c\ %*
set statusline+=%3*%{Spacing()}%*
