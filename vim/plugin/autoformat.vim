let g:autoformat_autoindent = 0
let g:autoformat_remove_trailing_spaces = 0
let g:autoformat_retab = 0

function! s:Format(doit)
  if !a:doit
    return
  endif

  try
    undojoin | Autoformat
  catch /E790/
    Autoformat
  endtry
endfunction

augroup setup_buildifier
  autocmd!

  autocmd BufNewFile,BufRead *.bzl let b:formatters_bzl = ['buildifierbzl']
  autocmd BufNewFile,BufRead *.star let b:formatters_bzl = []
  autocmd BufNewFile,BufRead BUILD,BUILD.bazel,*.BUILD,BUILD.* let b:formatters_bzl = ['buildifierbuild']
  autocmd BufNewFile,BufRead MODULE.bazel let b:formatters_bzl = ['buildifiermodule']
  autocmd BufNewFile,BufRead WORKSPACE,WORKSPACE.bazel let b:formatters_bzl = ['buildifierworkspace']

  autocmd FileType bzl autocmd BufWritePre <buffer> call s:Format(1)
  autocmd FileType c,cpp,cmake autocmd BufWritePre <buffer> call s:Format(get(b:, "format_on_save", 0))
  autocmd FileType lua autocmd BufWritePre <buffer> call s:Format(1)
augroup END
