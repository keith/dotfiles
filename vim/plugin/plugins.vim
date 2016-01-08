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

let g:syntastic_html_tidy_ignore_errors = [" proprietary attribute \"ng-"]
let g:syntastic_python_flake8_args = "--ignore = E501"
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_haskell_checkers = ['ghc-mod', 'hdevtools']
let g:hdevtools_options = '-g -Wall'

let g:syntastic_objc_compiler = 'clang'
let g:syntastic_objcpp_compiler = 'clang'
" let g:syntastic_objc_gcc_quiet_messages = { "regex": 'file not found' }
let g:syntastic_objc_check_header = 1

let s:pch_paths = ['*/*.pch', 'Resources/*/*.pch']
for path in s:pch_paths
  if !empty(glob(path))
    let b:syntastic_objc_cflags = '-include ' . expand(path)
    break
  endif
endfor

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
let g:clang_close_preview = 1
let g:clang_complete_macros = 1
let g:clang_complete_patterns = 1
let g:clang_library_path = '/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib'
let g:clang_snippets = 1
let g:clang_snippets_engine = 'ultisnips'
let g:clang_use_library = 1
let g:clang_auto_user_options = "compile_commands.json"
let g:clang_make_default_keymappings = 0

" python-mode
let g:pymode_breakpoint = 0

" delimitMate
" Currently doesn't work with vim-endwise
" https://github.com/tpope/vim-endwise/issues/11#issuecomment-38747137
let delimitMate_expand_cr = 1
let delimitMate_quotes = "\" '"

" Tab/Enter usage
" If the popup menu is open go back with shift-tab
inoremap <S-Tab> <C-R>=BackwardsTab()<CR>
function! BackwardsTab()
  if pumvisible()
    return "\<C-p>"
  endif

  return ""
endfunction

let g:ran_after_ultisnips = 0
function! RunAfterUltiSnips()
  snoremap <Tab> <Esc>:call UltiSnips#ExpandSnippetOrJump()<cr>
  let g:clang_snippets_engine = 'ultisnips'
  let g:ran_after_ultisnips = 1
endfunction

function! LoadedUltiSnips()
  if exists("s:loaded_ultisnips")
    return s:loaded_ultisnips
  endif

  if exists("g:did_UltiSnips_plugin") && g:did_UltiSnips_plugin
    let s:loaded_ultisnips = 1
    if !g:ran_after_ultisnips
      call RunAfterUltiSnips()
    endif
    return s:loaded_ultisnips
  else
    return 0
  endif
endfunction

inoremap <silent> <Tab> <C-R>=TabWrapper()<CR>
function! TabWrapper()
  if pumvisible()
    return "\<C-y>"
  else
    if LoadedUltiSnips()
      call UltiSnips#ExpandSnippetOrJump()
      if g:ulti_expand_or_jump_res != 0
        return ""
      endif
    endif

    if ForceTab() || empty(&omnifunc)
      return "\<Tab>"
    else
      return "\<C-x>\<C-o>"
    endif
  endif

  return "\<Tab>"
endfunction

" Ignore UltiSnips mappings, deal with it manually
let g:UltiSnipsExpandTrigger = "<F30>"
let g:UltiSnipsJumpForwardTrigger = "<F30>"
let g:UltiSnipsJumpBackwardTrigger = "<F30>"
let g:snips_author = "Keith Smiley"
let g:ulti_expand_or_jump_res = 0

" All of supertab in one function. #trolol
let g:invalid_tab_chars = ['^', '\^', '\s', '#', '/', '\\', '*']
function! ForceTab()
  let column = col('.') - 1
  let lastchar = getline('.')[column - 1]
  let iskeychar = lastchar =~ '\k' || lastchar == '.'
  let invalidchar = index(g:invalid_tab_chars, lastchar) < 0
  return !(column > 0 && (iskeychar && invalidchar))
endfunction

" Don't hide quotes in elzr/vim-json
" This is way too confusing, it makes me forget quotes on lines after others
let g:vim_json_syntax_conceal = 0

" vim-markdown
let g:markdown_fenced_languages = ['ruby', 'sh', 'objc', 'vim', 'swift']

" rails.vim
let g:rails_projections = {
      \ "config/routes.rb": {"command": "routes"},
      \ "spec/factories.rb": {"command": "factories"},
      \ "spec/factories/*.rb": {"command": "factories"}
    \ }

" vim-sort-motion
let g:sort_motion_flags = "ui"
