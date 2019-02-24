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

" vim-markdown
let g:markdown_fenced_languages = [
      \ 'objc',
      \ 'ruby',
      \ 'sh',
      \ 'swift',
      \ 'vim',
      \ 'yaml'
    \ ]

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

" Netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_list_hide =
      \ netrw_gitignore#Hide()
      \ . ',^\./'
      \ . ',^\.git/'

function! s:PrintNeomakeResult()
  echom printf('%s exited with %d',
        \ g:neomake_hook_context.jobinfo.maker.name,
        \ g:neomake_hook_context.jobinfo.exit_code)
endfunction

augroup neomake_config
  autocmd!
  autocmd! BufWritePost * Neomake

  autocmd User NeomakeJobFinished call s:PrintNeomakeResult()
augroup END

let g:neomake_error_sign = {'text': '>>', 'texthl': 'NeomakeErrorSign'}
let g:neomake_warning_sign = {'text': '>>', 'texthl': 'NeomakeWarningSign' }
let g:neomake_message_sign = {'text': 'm>', 'texthl': 'NeomakeMessageSign' }
let g:neomake_info_sign = {'text': 'i>', 'texthl': 'NeomakeInfoSign'}

let g:neomake_python_enabled_makers = ['python', 'flake8', 'pylint']
let g:neomake_ruby_enabled_makers = []
let g:neomake_java_enabled_makers = []
let g:neomake_open_list = 2

command! -nargs=+ Nrun call s:Nrun(<q-args>)
function! s:Nrun(args)
  let l:arguments = split(a:args)
  let l:executable = remove(l:arguments, 0)
  let l:arguments = join(l:arguments, ' ')

  let l:maker = {
        \ 'exe': l:executable,
        \ 'args': l:arguments,
        \ 'errorformat': &errorformat,
      \ }
  call neomake#Make(0, [l:maker])
endfunction

let g:ycm_filetype_whitelist = {
      \ 'python': 1,
      \ 'rust': 1,
    \ }

let g:highlightedyank_highlight_duration = 150

nnoremap <silent> <C-W>z :call zoom#toggle()<CR>

let s:lldb_projections = {
      \   "include/lldb/*.h": {"alternate": "source/{}.cpp"},
      \   "source/*.cpp": {"alternate": "include/lldb/{}.h"}
      \ }

let s:swift_projections = {
      \   "include/swift/*.h": {"alternate": "lib/{}.cpp"},
      \   "lib/*.cpp": {"alternate": "include/swift/{}.h"}
      \ }

let s:llvm_projections = {
      \   "include/llvm/*.h": {"alternate": "lib/{}.cpp"},
      \   "lib/*.cpp": {"alternate": "include/llvm/{}.h"}
      \ }

augroup custom_projectionist
  autocmd!

  autocmd User ProjectionistDetect
        \ if fnamemodify(getcwd(), ":t") == "lldb" |
        \   call projectionist#append(getcwd(), s:lldb_projections) |
        \ endif
  autocmd User ProjectionistDetect
        \ if fnamemodify(getcwd(), ":t") == "swift" |
        \   call projectionist#append(getcwd(), s:swift_projections) |
        \ endif
  autocmd User ProjectionistDetect
        \ if fnamemodify(getcwd(), ":t") == "llvm" |
        \   call projectionist#append(getcwd(), s:llvm_projections) |
        \ endif
augroup END

let g:ale_linters = {
      \ 'python': ['pyls'],
      \ 'ruby': [],
      \ 'rust': ['rls', 'cargo'],
      \ 'sh': ['shellcheck'],
      \ 'swift': ['swiftpm'],
    \ }
let g:ale_fixers = {'rust': ['rustfmt']}
let g:ale_fix_on_save = 1
let g:ale_open_list = 1

let g:black_linelength = 79
" Run Black on save if file .black exists.
if filereadable('.black')
  autocmd BufWritePre *.py execute ':silent Black'
endif

let g:tmux_navigator_disable_when_zoomed = 1
