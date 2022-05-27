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

let g:Illuminate_delay = 200

" Netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_list_hide =
      \ netrw_gitignore#Hide()
      \ . ',^\./'
      \ . ',^\.git/'

nnoremap <silent> <C-W>z :call zoom#toggle()<CR>

let g:gutentags_file_list_command = 'rg --files'
let g:gutentags_exclude_filetypes = ['gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail', 'git']

let g:tmux_navigator_disable_when_zoomed = 1

let g:hdevtools_stack = 1

let g:signify_sign_change = '~'
nmap [g <plug>(signify-prev-hunk)
nmap ]g <plug>(signify-next-hunk)

nnoremap [d :lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap ]d :lua vim.lsp.diagnostic.goto_next()<CR>

nnoremap <leader>g :Git blame<CR>
nnoremap <leader>v :.,.GBrowse<CR>
vnoremap <leader>v :.,.GBrowse<CR>

" The only vim-rsi mappings I need
inoremap <C-A> <C-O>^
cnoremap <C-A> <Home>
