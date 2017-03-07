" TagBar ------ {{{
nnoremap <silent> <Leader>b :TagbarToggle<CR>
" Order tags by their appearence in the file
let g:tagbar_sort = 0
let g:tagbar_type_go = {
  \ 'ctagstype' : 'go',
  \ 'kinds'     : [
    \ 'p:package',
    \ 'i:imports:1',
    \ 'c:constants',
    \ 'v:variables',
    \ 't:types',
    \ 'n:interfaces',
    \ 'w:fields',
    \ 'e:embedded',
    \ 'm:methods',
    \ 'r:constructor',
    \ 'f:functions'
  \ ],
  \ 'sro' : '.',
  \ 'kind2scope' : {
    \ 't' : 'ctype',
    \ 'n' : 'ntype'
  \ },
  \ 'scope2kind' : {
    \ 'ctype' : 't',
    \ 'ntype' : 'n'
  \ },
  \ 'ctagsbin'  : 'gotags',
  \ 'ctagsargs' : '-sort -silent'
\ }

let g:tagbar_type_objc = {
  \ 'ctagstype': 'objc',
  \ 'ctagsargs': [
    \ '-f',
    \ '-',
    \ '--excmd=pattern',
    \ '--extra=',
    \ '--format=2',
    \ '--fields=nksaSmt',
    \ '--objc-kinds=-N',
  \ ],
  \ 'sro': ' ',
  \ 'kinds': [
    \ 'c:constant',
    \ 'e:enum',
    \ 't:typedef',
    \ 'i:interface',
    \ 'P:protocol',
    \ 'p:property',
    \ 'I:implementation',
    \ 'M:method',
    \ 'g:pragma',
  \ ],
\ }
" }}}

let g:tagbar_type_swift = {
  \ 'ctagstype': 'swift',
  \ 'ctagsargs': [
    \ '-f',
    \ '-',
    \ '--excmd=pattern',
    \ '--extra=',
    \ '--format=2',
    \ '--fields=nksaSmt',
  \ ],
  \ 'sro': ' ',
  \ 'kinds': [
    \ 'c:class',
    \ 'e:enum',
    \ 'f:function',
    \ 'c:constant',
    \ 'P:protocol',
    \ 's:struct',
    \ 't:typealias',
    \ 'v:variable',
  \ ],
\ }
" }}}

" Syntastic ------ {{{
let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1

let g:syntastic_html_tidy_ignore_errors = [' proprietary attribute \"ng-']
let g:syntastic_python_checkers = ['pylint', 'python']
let g:syntastic_swift_checkers = ['swiftpm', 'swiftlint']
let g:syntastic_haskell_checkers = ['ghc-mod', 'hdevtools']
let g:hdevtools_options = '-g -Wall'

" Allow toggling of syntastic errors list
" http://stackoverflow.com/questions/17512794/toggle-error-location-panel-in-syntastic
function! ToggleErrors()
  " Check the total number of open windows
  let l:old_last_winnr = winnr('$')
  " Attempt to close the location list
  lclose
  " If there are still the same number of windows
  " Open the errors list
  if l:old_last_winnr == winnr('$')
    Errors
  endif
endfunction
nnoremap <leader>e :call ToggleErrors()<cr>
" }}}

" python-mode
let g:pymode_breakpoint = 0

" delimitMate
" Currently doesn't work with vim-endwise
" https://github.com/tpope/vim-endwise/issues/11#issuecomment-38747137
let g:delimitMate_expand_cr = 1
let g:delimitMate_quotes = "\" '"

" Tab/Enter usage
" If the popup menu is open go back with shift-tab
inoremap <S-Tab> <C-R>=BackwardsTab()<CR>
function! BackwardsTab()
  if pumvisible()
    return "\<C-p>"
  endif

  return ''
endfunction

inoremap <silent> <Tab> <C-R>=TabWrapper()<CR>
function! TabWrapper()
  if pumvisible()
    return "\<C-y>"
  else
    if ForceTab() || empty(&omnifunc)
      return "\<Tab>"
    else
      return "\<C-x>\<C-o>"
    endif
  endif

  return "\<Tab>"
endfunction

" All of supertab in one function. #trolol
let g:invalid_tab_chars = ['^', '\^', '\s', '#', '/', '\\', '*']
function! ForceTab()
  let l:column = col('.') - 1
  let l:lastchar = getline('.')[l:column - 1]
  let l:iskeychar = l:lastchar =~? '\k' || l:lastchar ==? '.'
  let l:invalidchar = index(g:invalid_tab_chars, l:lastchar) < 0
  return !(l:column > 0 && (l:iskeychar && l:invalidchar))
endfunction

" vim-markdown
let g:markdown_fenced_languages = ['ruby', 'sh', 'objc', 'vim', 'swift']

" vim-sort-motion
let g:sort_motion_flags = 'ui'

" vim-gnupg
let g:GPGDefaultRecipients = ['0x4C7167F8']
let g:GPGPreferArmor = 1

" Twitvim
let g:twitvim_count = 50
let g:twitvim_allow_multiline = 1

" jedi.vim
let g:jedi#show_call_signatures = 0

" codi.vim
function! s:Scratch(ft)
  execute 'edit ' . tempname()
  execute 'set filetype=' . a:ft
  execute 'Codi ' . a:ft
endfunction

command! -nargs=1 Scratch execute s:Scratch('<args>')

" Netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_list_hide =
      \ netrw_gitignore#Hide()
      \ . ',^\./'
      \ . ',^\.git/'

" completer.vim
let g:completor_whitelist = ['python']

let g:ale_linters = {'ruby': [], 'python': ['pylint']}

augroup neomake_config
  autocmd!
  autocmd! BufWritePost * Neomake
augroup END

let g:neomake_error_sign = {'text': '>>', 'texthl': 'NeomakeErrorSign'}
let g:neomake_warning_sign = {'text': '>>', 'texthl': 'NeomakeWarningSign' }
let g:neomake_message_sign = {'text': 'm>', 'texthl': 'NeomakeMessageSign' }
let g:neomake_info_sign = {'text': 'i>', 'texthl': 'NeomakeInfoSign'}

let g:neomake_python_enabled_makers = ['python', 'flake8', 'pylint']

let g:ycm_filetype_whitelist = {
      \ 'python': 1,
      \ 'rust': 1,
    \ }
