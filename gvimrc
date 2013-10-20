if has("gui_macvim")
  set guifont=Liberation_Mono:h13
elseif has("gui_gtk2")
  set guifont=Liberation\ Mono\ 11
end

" Using control shift brackets or control numbers to navigate tabs
map <C-S-]> :tabnext<cr>
map <C-S-[> :tabprevious<cr>

" Tab commands
nmap <C-tab>    :tabnext<cr>
nmap <C-S-tab>  :tabprevious<cr>

