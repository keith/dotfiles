setlocal expandtab
setlocal foldmethod=indent
setlocal shiftwidth=4
setlocal tabstop=4
setlocal keywordprg=pydoc

" Fix issue where comments cannot be moved from the first column with >>
setlocal nosmartindent

autocmd BufNewFile __doc__ nnoremap <buffer> <silent> q :bdelete<CR>

let b:format_on_save = filereadable('.black')
