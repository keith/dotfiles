let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

augroup setup_buildifier
  autocmd!

  autocmd BufNewFile,BufRead *.bzl let b:formatters_bzl = ['buildifierbzl']
  autocmd BufNewFile,BufRead BUILD,BUILD.bazel,*.BUILD,BUILD.* let b:formatters_bzl = ['buildifierbuild']
  autocmd BufNewFile,BufRead WORKSPACE let b:formatters_bzl = ['buildifierworkspace']

  autocmd BufWrite *.bzl,BUILD,BUILD.bazel,*.BUILD,BUILD.*,WORKSPACE :Autoformat
  " this is bad when working on llvm
  " autocmd BufWrite *.cpp,*.c,*.h :Autoformat
augroup END
