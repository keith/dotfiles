" Disable python2 support
let g:loaded_python_provider = 0
" Ignore local virtualenvs
" https://github.com/neovim/neovim/issues/1887#issuecomment-280653872
if exists("$VIRTUAL_ENV")
  let g:python_host_prog=substitute(system("which -a python | head -n2 | tail -n1"), "\n", '', 'g')
  let g:python3_host_prog=substitute(system("which -a python3 | head -n2 | tail -n1"), "\n", '', 'g')
endif

source ~/.vimrc

set fileignorecase
set guicursor=
set inccommand=nosplit
set omnifunc=v:lua.vim.lsp.omnifunc

autocmd TextYankPost * lua vim.highlight.on_yank {higroup="IncSearch", timeout=150, on_visual=true}
autocmd BufEnter,BufWinEnter,TabEnter *.rs :lua require'lsp_extensions'.inlay_hints{}

nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
" TODO: Ideally this would happen automatically when there were diagnostics
nnoremap <silent> <leader>l <cmd>lua vim.lsp.diagnostic.set_loclist()<CR>

nnoremap <silent> <leader>a <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <silent> <Leader>d <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>
nnoremap <silent> <leader>f <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <silent> <leader>h <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <leader>r <cmd>lua vim.lsp.buf.rename()<CR>

sign define LspDiagnosticsSignError text=E numhl=LspDiagnosticsDefaultError
sign define LspDiagnosticsSignWarning text=W numhl=LspDiagnosticsDefaultWarning
sign define LspDiagnosticsSignInformation text=I numhl=LspDiagnosticsDefaultInformation
sign define LspDiagnosticsSignHint text=H numhl=LspDiagnosticsDefaultHint
