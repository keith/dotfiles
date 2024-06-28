autocmd BufReadPost,BufNewFile *.bazelrc,*bazel.rc* set ft=bazelrc
autocmd BufReadPost,BufNewFile BUILD.*,*.star,WORKSPACE.bzlmod setfiletype bzl
