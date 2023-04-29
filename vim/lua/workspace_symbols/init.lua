local builtin = {}

-- Ref: https://github.com/tjdevries/lazy.nvim
local function require_on_exported_call(mod)
  return setmetatable({}, {
    __index = function(_, picker)
      return function(...)
        return require(mod)[picker](...)
      end
    end,
  })
end

builtin.lsp_dynamic_workspace_symbols =
  require_on_exported_call("workspace_symbols.workspace_symbols").custom_dynamic_workspace_symbols

local apply_config = function(mod)
  local pickers_conf = require("telescope.config").pickers
  for k, v in pairs(mod) do
    mod[k] = function(opts)
      opts = opts or {}
      opts.bufnr = opts.bufnr or vim.api.nvim_get_current_buf()
      opts.winnr = opts.winnr or vim.api.nvim_get_current_win()
      local pconf = pickers_conf[k] or {}
      local defaults = (function()
        if pconf.theme then
          return require("telescope.themes")["get_" .. pconf.theme](pconf)
        end
        return vim.deepcopy(pconf)
      end)()

      if pconf.mappings then
        defaults.attach_mappings = function(_, map)
          for mode, tbl in pairs(pconf.mappings) do
            for key, action in pairs(tbl) do
              map(mode, key, action)
            end
          end
          return true
        end
      end

      if pconf.attach_mappings and opts.attach_mappings then
        local opts_attach = opts.attach_mappings
        opts.attach_mappings = function(prompt_bufnr, map)
          pconf.attach_mappings(prompt_bufnr, map)
          return opts_attach(prompt_bufnr, map)
        end
      end

      v(vim.tbl_extend("force", defaults, opts))
    end
  end

  return mod
end

-- We can't do this in one statement because tree-sitter-lua docgen gets confused if we do
builtin = apply_config(builtin)
return builtin
