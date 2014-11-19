let s:colorPrefix = "cterm"
if has("gui_running")
  let s:colorPrefix = "gui"
endif

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
  execute 'hi User1 ' . s:colorPrefix . '=bold ' . s:colorPrefix . 'bg=' . color
  return '  ' . toupper(g:currentmode[a:mode]) . PasteString() . ' '
endfunction

function! Spacing()
  if &ro
    return ''
  endif

  if !empty(SyntasticStatuslineFlag())
    return ' ' . SyntasticStatuslineFlag() . ' '
  endif

  if mode() != "n" && exists("s:lastOutput")
    return s:lastOutput
  endif

  let trailing = search('\s$', 'nw')
  let output = ''
  if trailing
    let output = '  ' . printf('trailing[%s]', trailing) . ' '
  endif

  let s:lastOutput = output
  return output
endfunction

" This gets around gui vim's colors not being setup with the script is sourced
augroup StatusLine
  autocmd!
  autocmd VimEnter * call s:SetupColors()
augroup END

function! s:SetupColors()
  execute 'hi User2 ' . s:colorPrefix . '=bold ' . s:colorPrefix . 'bg=' . s:ColorForGroup('StatusLine')
  execute 'hi User3 ' . s:colorPrefix . '=bold ' . s:colorPrefix . 'bg=' . s:ColorForGroup('PreProc')
  execute 'hi User4 ' . s:colorPrefix . '=bold ' . s:colorPrefix . 'fg=' . s:ColorForGroup('Comment') . ' ' . s:colorPrefix . 'bg=' . s:ColorForGroup('StatusLine')
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
set statusline+=%4*%{tagbar#currenttag('%s\ <\ ','','')}
set statusline+=%{&ft}%*
set statusline+=%2*\ \ %P
set statusline+=\ :\ %l:
set statusline+=\ %c\ %*
set statusline+=%3*%{Spacing()}%*
