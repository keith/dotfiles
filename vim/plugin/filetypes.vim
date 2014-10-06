" CSS ------ {{{
augroup ft_css_sass
  autocmd!
  autocmd FileType css setlocal commentstring=/*\ %s\ */
  autocmd FileType css,scss.css setlocal foldmethod=marker foldmarker={,}
augroup END
" }}}

" Nginx ------ {{{
augroup ft_nginx
  autocmd!
  autocmd BufRead,BufNewFile /*/nginx/conf/*            setlocal filetype=nginx
  autocmd BufRead,BufNewFile /*/nginx/sites-available/* setlocal filetype=nginx
  autocmd FileType nginx setlocal foldmethod=marker foldmarker={,}
  autocmd FileType nginx setlocal commentstring=#\ %s
augroup END
" }}}

" Git ------ {{{
augroup ft_git
  autocmd!
  autocmd BufRead     *gitconfig     setlocal filetype=gitconfig
  autocmd FileType    gitcommit      setlocal spell textwidth=72
  autocmd FileType    gitcommit      setlocal completeopt+=preview
augroup END
" }}}

" Vim ------ {{{
augroup ft_help
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker isk+=:
  autocmd FileType vim setlocal makeprg=source\ %
  autocmd BufRead,BufNewFile *.vim/doc/*.txt setlocal filetype=help
  autocmd BufRead,BufNewFile vim-*/doc/*.txt setlocal filetype=help
  autocmd FileType help setlocal iskeyword+=-
  autocmd FileType help
        \ if !&readonly |
        \   setlocal spell autoindent formatoptions+=2n |
        \   execute "autocmd BufWrite * execute 'helptags ' . expand('%:h')" |
        \ endif
augroup END
" }}}

" Markdown files ------ {{{
function! Marked()
  silent !mark %
  redraw!
endfunction

augroup ft_markdown
  autocmd!
  if has("mac")
    autocmd FileType markdown  command! -buffer -bang Marked :call Marked()
  endif
  autocmd FileType markdown setlocal textwidth=72 spell
augroup END
" }}}

" Liquid files ------ {{{
augroup ft_liquid
  autocmd!
  autocmd FileType liquid setlocal textwidth=72 spell
augroup END
" }}}

" Mail files ------ {{{
augroup ft_mail
  autocmd BufRead,BufNewFile *mutt-* setfiletype mail
  autocmd FileType mail setlocal textwidth=78
  autocmd FileType mail setlocal spell
  autocmd FileType mail let g:airline#extensions#whitespace#enabled = 0
  autocmd FileType mail let g:airline#extensions#whitespace#checks = []
augroup END
" }}}

" Clojure files ------ {{{
augroup ft_clojure
  autocmd!
  autocmd FileType clojure setlocal foldmethod=marker foldmarker=(,)
  autocmd FileType clojure RainbowParenthesesActivate
  let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
  \ ]
  autocmd syntax clojure RainbowParenthesesLoadRound
  autocmd syntax clojure RainbowParenthesesLoadSquare
augroup END
" }}}

" Ruby files ------ {{{
augroup ft_ruby
  autocmd!
  autocmd FileType eruby,ruby set iskeyword+=?
  autocmd BufRead,BufNewFile *_spec.rb set syntax=rspec
  autocmd Syntax rspec setlocal makeprg=rspec\ %
  " Force Dispatch to run :Dispatch over :Make for better output
  autocmd Syntax rspec let b:dispatch=&makeprg
augroup END
" }}}

" Haskell files ------ {{{
augroup ft_haskell
  autocmd!
  autocmd FileType haskell setlocal makeprg=runhaskell\ %
augroup END
" }}}

" Quickfix ------ {{{
augroup ft_quickfix
  autocmd!
  autocmd BufReadPost quickfix nnoremap <silent> <buffer> q :cclose<CR>
  autocmd BufReadPost quickfix setlocal nocursorline
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
  autocmd FileType htmldjango    setlocal commentstring={#\ %s\ #}
  autocmd FileType htmldjango
    \ let b:surround_{char2nr("v")} = "{{ \r }}" |
    \ let b:surround_{char2nr("%")} = "{% \r %}"

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
