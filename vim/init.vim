" Disable python2 support
let g:loaded_python_provider = 0
" Ignore local virtualenvs
" https://github.com/neovim/neovim/issues/1887#issuecomment-280653872
if exists("$VIRTUAL_ENV")
  let g:python_host_prog=substitute(system("which -a python | head -n2 | tail -n1"), "\n", '', 'g')
  let g:python3_host_prog=substitute(system("which -a python3 | head -n2 | tail -n1"), "\n", '', 'g')
endif

source ~/.vimrc

set guicursor=
set inccommand=nosplit
set fileignorecase
