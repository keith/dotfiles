let g:autoformat_autoindent = 0
let g:autoformat_remove_trailing_spaces = 0
let g:autoformat_retab = 0

augroup setup_buildifier
  autocmd!

  autocmd BufNewFile,BufRead *.bzl let b:formatters_bzl = ['buildifierbzl']
  autocmd BufNewFile,BufRead BUILD,BUILD.bazel,*.BUILD,BUILD.* let b:formatters_bzl = ['buildifierbuild']
  autocmd BufNewFile,BufRead MODULE.bazel let b:formatters_bzl = ['buildifiermodule']
  autocmd BufNewFile,BufRead WORKSPACE,WORKSPACE.bazel let b:formatters_bzl = ['buildifierworkspace']
  autocmd FileType bzl autocmd BufWritePre <buffer> :Autoformat

  autocmd FileType c,cpp,cmake autocmd BufWritePre <buffer> if get(b:, "format_on_save", 0) | :Autoformat | endif

  autocmd BufWritePre *.lua :Autoformat
augroup END
