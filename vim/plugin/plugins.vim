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

let g:Illuminate_delay = 200
highlight illuminatedWord ctermbg=white ctermfg=black

" Netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_list_hide =
      \ netrw_gitignore#Hide()
      \ . ',^\./'
      \ . ',^\.git/'

let g:highlightedyank_highlight_duration = 150

nnoremap <silent> <C-W>z :call zoom#toggle()<CR>

let g:gutentags_file_list_command = 'rg --files'
let g:gutentags_exclude_filetypes = ['gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail', 'git']

let g:tmux_navigator_disable_when_zoomed = 1

let g:formatdef_buildifierbzl = "'buildifier -type bzl'"
let g:formatdef_buildifierbuild = "'buildifier -type build'"
let g:formatdef_buildifierworkspace = "'buildifier -type workspace'"
