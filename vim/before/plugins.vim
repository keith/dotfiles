" vim-commentary don't make backslash mappings
let g:commentary_map_backslash = 0

" vitality.vim
" If I need more before settings I will source another file
let g:vitality_fix_cursor = 0
" Need to make tmux FocusLost events work in Terminal.app
let g:vitality_always_assume_iterm = 1

" Make sure vim-sexp doesn't map any keys
" https://github.com/guns/vim-sexp/blob/master/plugin/sexp.vim#L92-L97
let g:sexp_filetypes = ""
