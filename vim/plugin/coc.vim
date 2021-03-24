autocmd Filetype gitcommit,gitconfig,gitrebase,gitsendemail,git let b:coc_suggest_disable = 1

let g:coc_global_extensions = [
      \ 'coc-clangd',
      \ 'coc-git',
      \ 'coc-json',
      \ 'coc-pyright',
      \ 'coc-rust-analyzer',
    \ ]

" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [d <Plug>(coc-diagnostic-prev)
nmap <silent> ]d <Plug>(coc-diagnostic-next)

nmap <silent> [g <Plug>(coc-git-prevchunk)
nmap <silent> ]g <Plug>(coc-git-nextchunk)

nnoremap <silent> <leader>h :call CocActionAsync('doHover')<cr>
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

command! A CocCommand clangd.switchSourceHeader

let g:lsp_cxx_hl_log_file = '/tmp/vim-lsp-cxx-hl.log'
let g:lsp_cxx_hl_verbose_log = 1

highlight default LspCxxHlGroupMemberVariable ctermfg=Magenta
