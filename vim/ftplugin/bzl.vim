setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

let g:formatdef_buildifierbuild = "'buildifier -type build'"
let g:formatdef_buildifierbzl = "'buildifier -type bzl'"
let g:formatdef_buildifiermodule = "'buildifier -type module'"
let g:formatdef_buildifierworkspace = "'buildifier -type workspace'"

function! WrapLabels() range
	silent! execute a:firstline .. "," .. a:lastline .. 's/^\( *\)\(.*\)$/\1"\2",/'
endfunction

noremap <leader>1 :call WrapLabels()<CR>
