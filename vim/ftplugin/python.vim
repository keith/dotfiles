setlocal expandtab
setlocal foldmethod=indent
setlocal omnifunc=pythoncomplete#Complete
setlocal shiftwidth=4
setlocal tabstop=4
setlocal keywordprg=pydoc

" Fix issue where comments cannot be moved from the first column with >>
setlocal nosmartindent

autocmd BufNewFile __doc__ nnoremap <buffer> <silent> q :bdelete<CR>
