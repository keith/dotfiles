setlocal iskeyword+=-
nnoremap <buffer> q :q<CR>

if !&readonly
  setlocal spell autoindent formatoptions+=2n
  execute "autocmd BufWrite * execute 'helptags ' . expand('%:h')"
endif
