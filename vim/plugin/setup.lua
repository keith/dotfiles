-- vim.lsp.set_log_level("debug")

local lspconfig = require "lspconfig"
local lsp_spinner = require "lsp_spinner"

lsp_spinner.setup {
  placeholder = "  ",
}

require("lsp-inlayhints").setup()

local function on_attach(client, bufnr)
  require("lsp_spinner").on_attach(client, bufnr)
  require("lsp-inlayhints").on_attach(client, bufnr)
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = {
    prefix = "",
    spacing = 2,
  },
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    "documentation",
    "detail",
    "additionalTextEdits",
  },
}
lsp_spinner.init_capabilities(capabilities)

local nvim_lsp = require "lspconfig"
local servers = {
  "bashls",
  "clangd",
  "cmake",
  "gopls",
  "graphql",
  "rust_analyzer",
  "terraformls",
  "zls",
}
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    capabilities = capabilities,
    on_attach = on_attach,
  }
end

-- https://github.com/microsoft/pyright/issues/128
require("lspconfig").pyright.setup {
  capabilities = capabilities,
  filetypes = { "python" },
  on_attach = on_attach,
  settings = {
    python = {
      analysis = {
        -- autoSearchPaths = true,
        -- useLibraryCodeForTypes = true,
        diagnosticMode = "openFilesOnly",
      },
    },
  },
}

require("lspconfig").sourcekit.setup {
  capabilities = capabilities,
  filetypes = { "swift" },
  on_attach = on_attach,
}

require("lsp_signature").on_attach {
  bind = true,
  hint_prefix = "",
  -- TODO: the border is huge, but these don't seem to work
  -- handler_opts = {
  --   border = "single"
  -- },
}

require("compe").setup {
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = "enable",
  throttle_time = 80,
  source_timeout = 200,
  incomplete_delay = 400,
  max_abbr_width = 100,
  max_kind_width = 100,
  max_menu_width = 100,
  documentation = true,

  source = {
    path = true,
    buffer = {
      ignored_filetypes = { "gitconfig", "gitcommit", "gitrebase", "git" },
    },
    nvim_lsp = true,
    nvim_lua = true,
  },
}

function has_highlights(lang)
  local supported = {
    c = true,
    cpp = true,
  }
  return supported[lang] ~= nil
end

require("nvim-treesitter.configs").setup {
  ensure_installed = "all",
  ignore_install = {
    "phpdoc", -- https://github.com/nvim-treesitter/nvim-treesitter/issues/2837
    "zig", -- https://github.com/nvim-treesitter/nvim-treesitter/issues/2049
  },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
    is_supported = has_highlights,
  },

  textsubjects = {
    enable = true,
    keymaps = {
      ["."] = "textsubjects-smart",
    },
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
