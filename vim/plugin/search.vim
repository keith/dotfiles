" Setup ag options for grepping
if executable("ag")
  set grepprg=ag\ --vimgrep\ $*
  set grepformat=%f:%l:%c:%m
endif

" Custom Ag command
command! -nargs=+ -bang Ag execute 'silent grep<bang> <args>' | copen | redraw!

" Find TODO and FIXME comments
silent! command Todo call TODOSearch()
function! TODOSearch()
  if executable("ag") && exists(":Ag") > 0
    Ag! "TODO\|FIXME"
  else
    noautocmd vimgrep /TODO\|FIXME/j ** | copen
  endif
endfunction

" Custom search motions
" From http://www.vimbits.com/bits/153
nnoremap <silent> s :set operatorfunc=<SID>GrepMotion<CR>g@
nmap <silent> S s$
xnoremap <silent> s :<C-U>call <SID>GrepMotion(visualmode())<CR>

function! s:CopyMotionForType(type)
  if a:type ==# "v"
    silent execute "normal! `<" . a:type . "`>y"
  elseif a:type ==# "char"
    silent execute "normal! `[v`]y"
  endif
endfunction

function! s:GrepMotion(type) abort
  let save = @@
  call s:CopyMotionForType(a:type)
  execute "normal! :Ag! --literal '" . shellescape(@@) . "'\<CR>"
  let @@ = save
endfunction
