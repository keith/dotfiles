autocmd BufNewFile,BufRead *.{m,pch} setlocal filetype=objc
autocmd BufNewFile,BufRead *.mm setlocal filetype=objcpp
autocmd BufRead *.pch setlocal syntax=pch
