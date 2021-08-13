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

nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> <leader>a <cmd>lua vim.lsp.buf.code_action()<CR>
" TODO: Ideally this would happen automatically when there were diagnostics
nnoremap <silent> <leader>l <cmd>lua vim.lsp.diagnostic.set_loclist()<CR>

nnoremap <silent> <leader>r <cmd>lua vim.lsp.buf.rename()<CR>

lua <<EOF
require'lspconfig'.bashls.setup{}
require'lspconfig'.clangd.setup{}
require'lspconfig'.gopls.setup{}
require'lspconfig'.pyright.setup{}
require'lspconfig'.rust_analyzer.setup{}
require'lspconfig'.sourcekit.setup{}
require'lspconfig'.terraformls.setup{}

require'lsp_signature'.on_attach({
  bind = true,
  hint_prefix = "",
  -- TODO: the border is huge, but these don't seem to work
  -- handler_opts = {
  --   border = "single"
  -- },
})

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = {
      ignored_filetypes = {"gitconfig", "gitcommit", "gitrebase", "git"};
    };
    nvim_lsp = true;
    nvim_lua = true;
  };
}
EOF

lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",
  -- TODO: Enable once I fix highlight groups
  -- highlight = {
  --   enable = true,
  -- },

  textsubjects = {
    enable = true,
    keymaps = {
      ['.'] = 'textsubjects-smart',
    }
  },

  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ic"] = "@call.outer",
      },
    },

    lsp_interop = {
      enable = true,
      peek_definition_code = {
        ["df"] = "@function.outer",
        ["dF"] = "@class.outer",
      },
    },
  },
}
EOF
