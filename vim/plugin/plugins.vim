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
let g:clang_auto_select = 0
let g:clang_close_preview = 1
let g:clang_compilation_database = ""
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
let g:clang_use_library = 1
let g:clang_user_options = s:compiler_options
let g:clang_make_default_keymappings = 0

" Clever-f
let g:clever_f_across_no_line = 1

" Dispatch.vim
nnoremap <leader>d :Dispatch<CR>

" investigate.vim
nnoremap <silent> K :call investigate#Investigate()<cr>
let g:investigate_use_dash = 1
let g:investigate_use_url_for_haskell = 1
" let g:investigate_command_for_python = "^i!pydoc ^s"
let g:investigate_dash_for_eruby="rails"

" vim-rooter
let g:rooter_manual_only = 1

" python-mode
let g:pymode_indent = 0
let g:pymode_breakpoint = 0
let g:pymode_doc = 0

" delimitMate
" Currently doesn't work with vim-endwise
" https://github.com/tpope/vim-endwise/issues/11#issuecomment-38747137
" let delimitMate_expand_cr = 1
let delimitMate_quotes = "\" '"

" Tab/Enter usage ------ {{{
" If the popup menu is open go back with shift-tab
inoremap <S-Tab> <C-R>=BackwardsTab()<CR>
function! BackwardsTab()
  if pumvisible()
    return "\<C-p>"
  endif

  return ""
endfunction

inoremap <Tab> <C-R>=TabWrapper()<CR>
snoremap <Tab> <Esc>:call UltiSnips#ExpandSnippetOrJump()<cr>

function! TabWrapper()
  if pumvisible()
    return "\<C-n>"
  else
    call UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res == 0
      if ForceTab() || empty(&omnifunc)
        return "\<Tab>"
      else
        return "\<C-x>\<C-o>"
      endif
    else
      return ""
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

let mappings = substitute(maparg("<CR>", 'i'), '<CR>', '', '')
execute 'imap <CR> <C-R>=EnterWrapper()<CR>' . mappings
function! EnterWrapper()
  if pumvisible()
    call UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res == 0
      return "\<CR>"
    else
      return "\<C-y>"
    endif
  endif

  call UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res > 0
    return "\<CR>"
  endif

  return "\<CR>"
endfunction

" neocomplete
" let g:neocomplete#enable_smart_case = 1
" let g:neocomplete#data_directory = '~/.cache/neocomplete'

" clang_complete compatibility from :h neocomplete
" if !exists('g:neocomplete#force_omni_input_patterns')
"   let g:neocomplete#force_omni_input_patterns = {}
" endif
" let g:neocomplete#force_overwrite_completefunc = 1
" let g:neocomplete#force_omni_input_patterns.objc =
"       \ '\v(\.|->|\s|\[|:|,)*\w*[^\.\.]'
      " \ '[^.[:digit:] *\t]\%(\.\|->\)\w*'
" Disable NeoComplete sometimes
" autocmd BufNewFile,BufRead * call EnableNeoComplete()
" function! EnableNeoComplete()
"   if &ft =~ 'gitcommit\|mail\|objc\|python'
"     NeoCompleteLock
"     return
"   end

"   NeoCompleteEnable
" endfunction

" All of supertab in one function. #trolol
let g:invalid_tab_chars = ['^', '\^', '\s', '#', '/', '\\', '*']
function! ForceTab()
  let column = col('.') - 1
  let lastchar = getline('.')[column - 1]
  let iskeychar = lastchar =~ '\k' || lastchar == '.'
  let invalidchar = index(g:invalid_tab_chars, lastchar) < 0
  return !(column > 0 && (iskeychar && invalidchar))
endfunction
" }}}

" vim-slime
let g:slime_target = "tmux"

" vim-arpeggio
silent! call arpeggio#map('i', '', 0, 'jk', '<Esc>')

" javascript-libraries-syntax.vim
let g:used_javascript_libs = 'angularjs,jasmine'

" Don't hide quotes in elzr/vim-json
" IMO this is way too confusing, it makes me forget quotes
" On lines after others
let g:vim_json_syntax_conceal = 0

" vim-markdown
let g:markdown_fenced_languages = ['ruby', 'sh', 'objc']

" tern
let g:tern_map_prefix = '<Leader>'
