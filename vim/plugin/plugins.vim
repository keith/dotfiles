" CTRL-P
let g:ctrlp_show_hidden = 1
unlet! g:ctrlp_user_command
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

" Airline
let g:airline_theme = 'custom'
let g:airline_left_sep = ''
let g:airline_right_sep = ''

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
    \ '--options=' . expand('~/.vim/objctags'),
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

" Syntastic ------ {{{
let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1

let g:syntastic_html_tidy_ignore_errors = [" proprietary attribute \"ng-"]
let g:syntastic_python_flake8_args = "--ignore = E501"
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_haskell_checkers = ['ghc-mod', 'hdevtools']
let g:hdevtools_options = '-g -Wall'

let s:compiler_options = '-std=gnu99 -fobjc-arc -fmodules'
let g:syntastic_objc_compiler = 'clang'
let g:syntastic_objcpp_compiler = 'clang'
let g:syntastic_objc_gcc_quiet_messages = { "regex": 'file not found' }
let g:syntastic_objc_check_header = 1
let g:syntastic_objc_compiler_options = s:compiler_options
let s:module_cache = expand('$HOME') . '/Library/Developer/Xcode/DerivedData/ModuleCache'
if isdirectory(s:module_cache)
  let g:syntastic_objc_compiler_options .= ' -fmodules-cache-path=' . s:module_cache
endif
let s:pch_path = '*/*.pch'
if !empty(glob(s:pch_path))
  let b:syntastic_objc_cflags = '-include ' . s:pch_path
endif

" Allow toggling of syntastic errors list
" http://stackoverflow.com/questions/17512794/toggle-error-location-panel-in-syntastic
function! ToggleErrors()
  " Check the total number of open windows
  let old_last_winnr = winnr('$')
  " Attempt to close the location list
  lclose
  " If there are still the same number of windows
  " Open the errors list
  if old_last_winnr == winnr('$')
    Errors
  endif
endfunction
nnoremap <leader>e :call ToggleErrors()<cr>
" }}}

" clang_complete
let g:clang_auto_select = 2
let g:clang_close_preview = 1
let g:clang_complete_auto = 0
let g:clang_complete_copen = 0
let g:clang_complete_macros = 1
let g:clang_complete_patterns = 1
let g:clang_conceal_snippets = 1
let g:clang_hl_errors = 0
let g:clang_jumpto_back_key = "<C-5>"
let g:clang_library_path = '/Library/Developer/CommandLineTools/usr/lib'
let g:clang_periodic_quickfix = 0
let g:clang_snippets = 1
let g:clang_snippets_engine = 'ultisnips'
let g:clang_user_options = s:compiler_options

" Clever-f
let g:clever_f_across_no_line = 1

" Dispatch.vim
nnoremap <leader>d :Dispatch<CR>

" investigate.vim
nnoremap <silent> K :call investigate#Investigate()<cr>
let g:investigate_use_dash = 1
let g:investigate_use_url_for_haskell = 1
let g:investigate_command_for_python = "^i!pydoc ^s"
let g:investigate_dash_for_eruby="rails"

" vim-rooter
let g:rooter_manual_only = 1

" python-mode
let g:pymode_indent = 0
let g:pymode_breakpoint = 0

" delimitMate
" Currently doesn't work with vim-endwise
" https://github.com/tpope/vim-endwise/issues/11#issuecomment-38747137
" let delimitMate_expand_cr = 1

" neocomplete
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
" clang_complete compatibility from :h neocomplete
if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_overwrite_completefunc = 1
let g:neocomplete#force_omni_input_patterns.c =
    \ '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.cpp =
    \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.objc =
    \ '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.objcpp =
    \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'

" Tab usage ------ {{{
" If the popup menu is open go back with shift-tab
inoremap <S-Tab> <C-R>=BackwardsTab()<CR>
function! BackwardsTab()
  if pumvisible()
    return "\<C-p>"
  endif

  return ""
endfunction

" neocomplete + ultisnips + clangcomplete + normal tab usage...
inoremap <Tab> <C-R>=Ulti_ExpandOrJump_and_getRes()<CR>
" Hierarchy
" If the popup menu is visible, iterate through it
" Otherwise attempt to expand or jump with ultisnips
" If that fails check if you should complete or tab at the cursor
let g:ulti_expand_or_jump_res = 0
function! Ulti_ExpandOrJump_and_getRes()
  if pumvisible()
    return "\<C-n>"
  else
    call UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res > 0
      return ""
    else
      if Should_tab()
        return "\<Tab>"
      else
        return "\<C-x>\<C-o>"
      endif
    endif
  endif
endfunction

" All of supertab in one function. #trolol
let g:invalid_tab_chars = ['^', '\^', '\s', '#', '/', '\\', '*']
function! Should_tab()
  let l:col = col('.') - 1
  let l:lastchar = getline('.')[l:col - 1]
  return !(l:col > 0 && (getline('.')[l:col - 1] =~ '\k'
        \ && index(g:invalid_tab_chars, l:lastchar) < 0))
endfunction
" }}}
