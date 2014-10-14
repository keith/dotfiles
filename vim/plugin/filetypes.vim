" Nginx ------ {{{
augroup ft_nginx
  autocmd!
  autocmd BufRead,BufNewFile /*/nginx/conf/*            setlocal filetype=nginx
  autocmd BufRead,BufNewFile /*/nginx/sites-available/* setlocal filetype=nginx
  autocmd FileType nginx setlocal foldmethod=marker foldmarker={,}
  autocmd FileType nginx setlocal commentstring=#\ %s
augroup END
" }}}

" ObjC stuff ------ {{{
augroup ft_objc
  autocmd!
  autocmd BufNewFile,BufRead *.{h,m,pch} setlocal filetype=objc
  autocmd BufRead *{Test,Spec}.m call specta#TestSyntaxSetup()
  autocmd FileType objc setlocal tabstop=4 shiftwidth=4 expandtab completeopt=menu
  autocmd FileType swift setlocal textwidth=0

  " Don't force lines starting with # to column 0 in pch files
  autocmd BufRead *.pch setlocal syntax=pch
  autocmd Syntax    pch setlocal indentkeys-=0#
augroup END
" }}}

" Various filetype settings ------ {{{
augroup ft_settings
  autocmd!
  autocmd FileType json          setlocal foldmethod=syntax
  autocmd FileType make,go,php   setlocal tabstop=4 shiftwidth=4 noexpandtab
  autocmd FileType dcl           setlocal filetype=apache
  autocmd FileType apache        setlocal commentstring=#\ %s
  autocmd FileType text          setlocal formatoptions+=t2n

  " Set normal completion functions
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType ruby setlocal omnifunc=
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

  " Fix issue where comments cannot be moved from the first column with >>
  autocmd FileType python        setlocal tabstop=4 shiftwidth=4 expandtab nosmartindent foldmethod=indent

  " Comment string settings
  if empty(&commentstring) | setlocal commentstring=#\ %s | endif
  autocmd FileType cf setlocal commentstring=<!---\ %s\ --->
  autocmd FileType conkyrc,crontab setlocal commentstring=#\ %s
  autocmd FileType c,cpp,go,objc,php setlocal commentstring=//\ %s

  " Put the cursor on a blank line with brackets
  " Using execute to not break highlighting for open brackets
  " autocmd FileType c,cpp,go,objc,php,css,sass.css autocmd BufRead *
  "       \ execute "inoremap {<CR> {<CR>}<ESC>O"

  " Save files on some focus lost events, like switching splits
  autocmd BufLeave,FocusLost * silent! wall

  " Don't auto insert a comment when using O/o for a newline
  autocmd BufRead,BufReadPost,Syntax,VimEnter * set formatoptions-=o

  " Return to the same position you left the file in
  autocmd BufReadPost * call PositionRecall()

  autocmd CursorHold * checktime
augroup END
" }}}
