" vim-markdown
let g:markdown_fenced_languages = [
      \ 'bzl',
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

" Netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
" Use my custom open executable
let g:netrw_browsex_viewer='open'
let g:netrw_list_hide =
      \ '^\./'
      \ . ',^\.git/'

nnoremap <silent> <C-W>z :call zoom#toggle()<CR>

let g:gutentags_file_list_command = 'rg --files'
let g:gutentags_ctags_exclude = ['*/bazel-out/*', '*/bazel-bin/*']
let g:gutentags_exclude_filetypes = ['gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail', 'git']

let g:tmux_navigator_disable_when_zoomed = 1

let g:sleuth_bzl_heuristics = 0

let g:hdevtools_stack = 1

let g:signify_sign_change = '~'
nmap [g <plug>(signify-prev-hunk)
nmap ]g <plug>(signify-next-hunk)

nnoremap [d :lua vim.diagnostic.goto_prev()<CR>
nnoremap ]d :lua vim.diagnostic.goto_next()<CR>

nnoremap <leader>g :Git blame<CR>
nnoremap <leader>v :.,.GBrowse<CR>
vnoremap <leader>v :.,.GBrowse<CR>

" The only vim-rsi mappings I need
inoremap <C-A> <C-O>^
cnoremap <C-A> <Home>

let g:oscyank_silent = 1
function! s:VimOSCYankPostCallback(event)
  if a:event.operator == 'y' && index(['', '+', '*'], a:event.regname) != -1
    call OSCYankRegister(a:event.regname)
  endif
endfunction

augroup VimOSCYankPost
  autocmd!
  autocmd TextYankPost * call s:VimOSCYankPostCallback(v:event)
augroup END
