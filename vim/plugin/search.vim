" Setup ag options for grepping
if executable("ag")
  set grepprg=ag\ --vimgrep\ $*
  set grepformat=%f:%l:%c:%m
endif

" Custom Ag command
command! -nargs=+ -bang Ag execute 'silent grep<bang> <args>' | copen | redraw!

" Search for the word under the cursor with Ag
nnoremap sag :Ag! <cword><CR>

" Find TODO and FIXME comments
silent! command Todo call TODOSearch()
function! TODOSearch()
  if executable("ag") && exists(":Ag") > 0
    Ag! "TODO\|FIXME"
  else
    noautocmd vimgrep /TODO\|FIXME/j ** | copen
  endif
endfunction
