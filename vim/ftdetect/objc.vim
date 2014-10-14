autocmd BufNewFile,BufRead *.{h,m,pch} setlocal filetype=objc
autocmd BufRead *.pch setlocal syntax=pch
autocmd BufRead *{Test,Spec}.m call specta#TestSyntaxSetup()
