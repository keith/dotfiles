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

let g:Illuminate_delay = 200
highlight illuminatedWord ctermbg=white ctermfg=black

" Netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_list_hide =
      \ netrw_gitignore#Hide()
      \ . ',^\./'
      \ . ',^\.git/'

let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'
let g:ycm_filetype_whitelist = {
      \ 'cpp': 1,
      \ 'python': 1,
    \ }

" Let clangd fully control code completion
let g:ycm_clangd_uses_ycmd_caching = 0
" Use installed clangd, not YCM-bundled clangd which doesn't get updates.
let g:ycm_clangd_binary_path = '/usr/local/opt/llvm/bin/clangd'
let g:ycm_clangd_args = ['-log=verbose', '-pretty']

let g:highlightedyank_highlight_duration = 150

nnoremap <silent> <C-W>z :call zoom#toggle()<CR>

let g:gutentags_file_list_command = 'rg --files'
let g:gutentags_exclude_filetypes = ['gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail', 'git']

let g:tmux_navigator_disable_when_zoomed = 1
