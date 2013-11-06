if has("gui_macvim")
  set guifont=Liberation_Mono:h13
elseif has("gui_gtk2")
  set guifont=Liberation\ Mono\ 11
end

" Using control shift brackets or control numbers to navigate tabs
noremap <C-S-]> :tabnext<cr>
noremap <C-S-[> :tabprevious<cr>

" Tab commands
nnoremap <C-tab>    :tabnext<cr>
nnoremap <C-S-tab>  :tabprevious<cr>

" Map home and end keys to the correct things
noremap <Home> gg
noremap <End>  G
noremap <PageUp> <C-b>
noremap <PageDown> <C-f>

