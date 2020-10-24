let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'
let g:ycm_filetype_whitelist = {}

" Let clangd fully control code completion
let g:ycm_clangd_uses_ycmd_caching = 0
" Use installed clangd, not YCM-bundled clangd which doesn't get updates.
let g:ycm_clangd_binary_path = '/usr/local/opt/llvm/bin/clangd'
let g:ycm_clangd_args = ['-log=verbose', '-pretty']
