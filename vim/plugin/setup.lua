-- vim.lsp.set_log_level("debug")

vim.o.termguicolors = false
vim.lsp.inlay_hint.enable()
vim.diagnostic.config { virtual_text = true }

local capabilities = require("cmp_nvim_lsp").default_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    "documentation",
    "detail",
    "additionalTextEdits",
  },
}

vim.lsp.config("*", {
  capabilities = capabilities,
})

local cmp = require "cmp"
cmp.setup {
  preselect = cmp.PreselectMode.None,
  snippet = {
    expand = function(args)
      vim.snippet.expand(args.body)
    end,
  },
  mapping = {
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<CR>"] = cmp.mapping.confirm { select = false },
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

local servers = {
  "bashls",
  "bazelrc_lsp",
  "clangd",
  "cmake",
  "gopls",
  "graphql",
  "lua_ls",
  "mojo",
  "pyright",
  "rust_analyzer",
  "sourcekit",
  "starpls",
  "tblgen_lsp_server",
  "terraformls",
  "ty",
  "zls",
}
for _, server in ipairs(servers) do
  vim.lsp.enable(server)
end

vim.lsp.config("sourcekit", {
  filetypes = { "swift", "objc", "objcpp" },
})

-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/configs.md#lua_ls
vim.lsp.config("lua_ls", {
  on_init = function(client)
    if client.workspace_folders then
      local path = client.workspace_folders[1].name
      if
        path ~= vim.fn.stdpath "config"
        and (vim.uv.fs_stat(path .. "/.luarc.json") or vim.uv.fs_stat(path .. "/.luarc.jsonc"))
      then
        return
      end
    end

    client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
      runtime = {
        version = "LuaJIT",
        path = {
          "lua/?.lua",
          "lua/?/init.lua",
        },
      },
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME,
        },
      },
    })
  end,
  settings = {
    Lua = {},
  },
})

local ignore_install = {
  phpdoc = true, -- https://github.com/nvim-treesitter/nvim-treesitter/issues/2837
  zig = true, -- https://github.com/nvim-treesitter/nvim-treesitter/issues/2049
}

local parsers = {}
for _, lang in ipairs(require("nvim-treesitter.config").get_available()) do
  if not ignore_install[lang] then
    table.insert(parsers, lang)
  end
end
require("nvim-treesitter").install(parsers)

local highlight_langs = { c = true, cpp = true }
vim.api.nvim_create_autocmd("FileType", {
  callback = function(args)
    local lang = vim.treesitter.language.get_lang(vim.bo[args.buf].filetype)
    if lang and highlight_langs[lang] then
      pcall(vim.treesitter.start, args.buf, lang)
    else
      pcall(vim.treesitter.stop, args.buf)
    end
  end,
})

require("gitsigns").setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
    untracked = { text = "?" },
  },
  signs_staged = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
    untracked = { text = "?" },
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
    "log",
    "tablegen",
    "cpp", -- Too slow for large files like llvm-project/clang/lib/Driver/ToolChains/Clang.cpp
  },
}

require("treesj").setup()
require("fidget").setup {}
