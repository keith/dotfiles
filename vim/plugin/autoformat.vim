let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

augroup setup_buildifier
  autocmd!

  " TODO: This is broken if you have a build file and then a json file? idk
  autocmd BufNewFile,BufRead *.bzl let g:formatters_bzl = ['buildifierbzl']
  autocmd BufNewFile,BufRead BUILD,BUILD.bazel,*.BUILD,BUILD.* let g:formatters_bzl = ['buildifierbuild']
  autocmd BufNewFile,BufRead WORKSPACE let g:formatters_bzl = ['buildifierworkspace']

  autocmd BufWrite *.bzl,BUILD,BUILD.bazel,*.BUILD,BUILD.*,WORKSPACE :Autoformat
  " this is bad when working on llvm
  " autocmd BufWrite *.cpp,*.c,*.h :Autoformat
augroup END
