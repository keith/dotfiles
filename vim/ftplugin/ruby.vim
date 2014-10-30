setlocal iskeyword+=?
setlocal omnifunc=

command! HashConvert call HashConvert()
function! HashConvert()
  normal mi
  %s/:\(\w*\)\(\s*\)=> /\1:\2/
  let @/=""
  normal `i
endfunction

" In theory this speeds up ruby launch time
" http://stackoverflow.com/a/12141458/902968
let s:location = expand('$HOME/.rbenv/shims')
if filereadable(s:location)
  let g:ruby_path = s:location
else
  let s:output = system('rvm current')
  if v:shell_error == 0
    let g:ruby_path = s:output
  endif
end
