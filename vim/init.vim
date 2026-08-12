" Disable python2 support
let g:loaded_python_provider = 0

" Cache compiled Lua modules between launches.
lua vim.loader.enable()

" Start slow subprocess queries now and collect the results further down, so
" they run while the rest of the config loads instead of blocking startup on
" each one serially.
lua << EOF
local ok, proc = pcall(vim.system, { "get-appearance" }, { text = true })
local startup_proc = ok and proc or nil

-- Sets 'background' to match the system appearance. The first call below
-- consumes the query spawned above, by then it has usually finished so this
-- doesn't block. Later calls (FocusGained) re-query asynchronously.
function _G.keith_sync_background()
  if startup_proc then
    local out = startup_proc:wait()
    startup_proc = nil
    vim.o.background = out.stdout
    return
  end

  pcall(vim.system, { "get-appearance" }, { text = true }, function(out)
    if out.code == 0 and (out.stdout == "dark" or out.stdout == "light") then
      vim.schedule(function()
        vim.o.background = out.stdout
      end)
    end
  end)
end

-- Ignore local virtualenvs
-- https://github.com/neovim/neovim/issues/1887#issuecomment-280653872
-- TODO: there is some error with this somehow i loaded a virtualenv but the python wasn't in my path correctly
if vim.env.VIRTUAL_ENV then
  _G.keith_python_procs = {}
  for _, python in ipairs { "python", "python3" } do
    _G.keith_python_procs[python] = vim.system {
      vim.o.shell,
      "-c",
      "which -a " .. python .. " | head -n2 | tail -n1",
    }
  end
end
EOF

lua require('plugins')

if exists("$VIRTUAL_ENV")
  lua vim.g.python_host_prog = (_G.keith_python_procs.python:wait().stdout:gsub("\n", ""))
  lua vim.g.python3_host_prog = (_G.keith_python_procs.python3:wait().stdout:gsub("\n", ""))
  lua _G.keith_python_procs = nil
endif

" Set 'background' before the vimrc loads the colorscheme
call v:lua.keith_sync_background()

source ~/.vimrc

augroup nvim_theme
  autocmd!
  " Update asynchronously so regaining focus doesn't block on a subprocess
  autocmd FocusGained * silent! call v:lua.keith_sync_background()
augroup END

set fileignorecase
set guicursor=
set inccommand=nosplit
set omnifunc=v:lua.vim.lsp.omnifunc

autocmd TextYankPost * lua vim.highlight.on_yank {higroup="IncSearch", timeout=150, on_visual=true}

nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
" TODO: Testing these out, better mappings needed
nnoremap <silent> <leader>c <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> <leader>i <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
" TODO: Ideally this would happen automatically when there were diagnostics
nnoremap <silent> <leader>l <cmd>lua vim.lsp.diagnostic.setloclist()<CR>

nnoremap <silent> <leader>a <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <silent> <Leader>d <cmd>lua vim.diagnostic.open_float()<CR>
nnoremap <silent> <leader>f <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <silent> <leader>h <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <leader>r <cmd>lua vim.lsp.buf.rename()<CR>

sign define DiagnosticSignError text=E numhl=DiagnosticSignError
sign define DiagnosticSignWarn text=W numhl=DiagnosticSignWarn
sign define DiagnosticSignInfo text=I numhl=DiagnosticSignInfo
sign define DiagnosticSignHint text=H numhl=DiagnosticSignHint
