if filereadable('.rustfmt')
  let g:rustfmt_autosave = 1
endif

nmap <leader>f <Plug>(rust-def)
nmap K <Plug>(rust-doc)

if executable('rls')
  set omnifunc=lsp#complete
  au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'stable', 'rls']},
        \ 'whitelist': ['rust'],
        \ })
endif
