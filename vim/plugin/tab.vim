" Tab/Enter usage
" If the popup menu is open go back with shift-tab
inoremap <S-Tab> <C-R>=<SID>BackwardsTab()<CR>
function! s:BackwardsTab()
  if pumvisible()
    return "\<C-p>"
  endif

  return ''
endfunction

inoremap <silent> <Tab> <C-R>=<SID>TabWrapper()<CR>
function! s:TabWrapper()
  if pumvisible()
    return "\<C-y>"
  else
    if s:ForceTab() || empty(&omnifunc)
      return "\<Tab>"
    else
      return "\<C-x>\<C-o>"
    endif
  endif

  return "\<Tab>"
endfunction

" Check if you should use a tab based on special characters
let g:invalid_tab_chars = ['^', '\^', '\s', '#', '/', '\\', '*']
function! s:ForceTab()
  let l:column = col('.') - 1
  let l:lastchar = getline('.')[l:column - 1]
  let l:iskeychar = l:lastchar =~? '\k' || l:lastchar ==? '.'
  let l:invalidchar = index(g:invalid_tab_chars, l:lastchar) < 0
  return !(l:column > 0 && (l:iskeychar && l:invalidchar))
endfunction
