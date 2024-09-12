autocmd BufReadPost,BufNewFile *.bazelrc,*bazel.rc* set ft=bazelrc
autocmd BufReadPost,BufNewFile BUILD.*,*.star,*.sky,WORKSPACE.bzlmod setfiletype bzl
