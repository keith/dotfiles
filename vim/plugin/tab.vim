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
  echom "1"
  let a = luaeval("require('cmp').visible()")
  if a
  " if pumvisible() || lua require('cmp').visible()


  echom "2"
    luaeval("require'cmp'.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true })")
    " call compe#confirm("\<C-y>")
    return
  else
  echom "3"
    if s:ForceTab() || empty(&omnifunc)
  echom "4"
      return "\<Tab>"
    else
  echom "5"
      return "\<C-x>\<C-o>"
    endif
  endif
  echom "6"

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
