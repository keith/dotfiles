-- vim.lsp.set_log_level("debug")

vim.o.termguicolors = false

local function on_attach(client, bufnr)
  if client.server_capabilities.inlayHintProvider then
    vim.lsp.inlay_hint.enable(true, { bufnr })
  end
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = {
    prefix = "",
    spacing = 2,
  },
})

local capabilities = require("cmp_nvim_lsp").default_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    "documentation",
    "detail",
    "additionalTextEdits",
  },
}

local cmp = require "cmp"
cmp.setup {
  snippet = {
    expand = function(args)
      vim.snippet.expand(args.body)
    end,
  },
  mapping = {
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<CR>"] = cmp.mapping.confirm { select = true },
  },
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
  }, {
    { name = "buffer" },
  }, {
    { name = "path" },
  }, {
    { name = "nvim_lsp_signature_help" },
  }),
}

vim.keymap.set({ "i", "s" }, "<Tab>", function()
  if vim.snippet.active { direction = 1 } then
    return "<Cmd>lua vim.snippet.jump(1)<CR>"
  elseif cmp.visible() then
    return '<Cmd>lua require("cmp").confirm({ select = true })<CR>'
  else
    return "<Tab>"
  end
end, { expr = true })

vim.keymap.set({ "i", "s" }, "<S-Tab>", function()
  if vim.snippet.active { direction = -1 } then
    return "<Cmd>lua vim.snippet.jump(-1)<CR>"
  elseif cmp.visible() then
    return '<Cmd>lua require("cmp").select_prev_item()<CR>'
  else
    return "<S-Tab>"
  end
end, { expr = true })

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ "/", "?" }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = "buffer", max_item_count = 4 },
  },
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline", max_item_count = 10 },
  }),
  matching = { disallow_symbol_nonprefix_matching = false },
})

cmp.setup.filetype("git", { sources = {} })
cmp.setup.filetype("gitcommit", { sources = {} })
cmp.setup.filetype("gitconfig", { sources = {} })
cmp.setup.filetype("gitrebase", { sources = {} })
cmp.setup.filetype("markdown", { sources = {} })

local nvim_lsp = require "lspconfig"
local servers = {
  "bashls",
  "bazelrc-lsp",
  "clangd",
  "cmake",
  "gopls",
  "graphql",
  "mojo",
  "rust_analyzer",
  "starpls",
  "tblgen_lsp_server",
  "terraformls",
  "zls",
}
for _, server in ipairs(servers) do
  local lsp = require "lspconfig"
  if vim.fn.executable(lsp[server].document_config.default_config.cmd[1]) == 1 then
    nvim_lsp[server].setup {
      capabilities = capabilities,
      on_attach = on_attach,
    }
  end
end

-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#lua_ls
require("lspconfig").lua_ls.setup {
  capabilities = capabilities,
  on_attach = on_attach,
  settings = {
    Lua = {
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME,
        },
      },
      telemetry = { enable = false },
      hint = { enable = true },
    },
  },
}

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

local function has_highlights(lang)
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
      -- TODO: These conflict with deleting things while visually selected
      -- peek_definition_code = {
      --   ["df"] = "@function.outer",
      --   ["dF"] = "@class.outer",
      -- },
    },
  },
}

require("illuminate").configure {
  providers = {
    "lsp",
    "treesitter",
    "regex",
  },
  delay = 200,
  filetypes_denylist = {
    "bzl",
    "fugitive",
  },
}

require("treesj").setup()
require("fidget").setup {}
