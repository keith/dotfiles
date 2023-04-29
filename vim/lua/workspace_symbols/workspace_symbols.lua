-- Initially copied from:
-- https://github.com/nvim-telescope/telescope.nvim/blob/b5833a682c511885887373aad76272ad70f7b3c2/lua/telescope/builtin/__lsp.lua
-- but changed to exclude matches for files that aren't in the pwd. Useful for
-- monorepos when you focus within a subdirectory

local channel = require("plenary.async.control").channel
local sorters = require "telescope.sorters"
local conf = require("telescope.config").values
local finders = require "telescope.finders"
local make_entry = require "telescope.make_entry"
local pickers = require "telescope.pickers"

local function get_workspace_symbols_requester(bufnr, root)
  local cancel = function() end

  return function(prompt)
    local tx, rx = channel.oneshot()
    cancel()
    _, cancel = vim.lsp.buf_request(bufnr, "workspace/symbol", { query = prompt }, tx)

    -- Handle 0.5 / 0.5.1 handler situation
    local err, res = rx()
    assert(not err, err)

    local locations = vim.lsp.util.symbols_to_items(res or {}, bufnr) or {}
    return vim.tbl_filter(function(item)
      return string.find(item.filename, root) ~= nil
    end, locations)
  end
end

local function root_regex()
  if os.execute "git check-ignore ." == 0 then
    root = io.popen("git rev-parse --show-toplevel"):read()
  else
    root = io.popen("pwd"):read()
  end

  return "^" .. root:gsub("%-", "%%-") .. "/"
end

lsp_custom_dynamic_workspace_symbols = function(opts)
  pickers.new(opts, {
    prompt_title = "LSP Dynamic Workspace Symbols",
    finder = finders.new_dynamic {
      entry_maker = make_entry.gen_from_lsp_symbols(opts),
      fn = get_workspace_symbols_requester(opts.bufnr, root_regex()),
    },
    previewer = conf.qflist_previewer(opts),
    sorter = sorters.highlighter_only(opts),
  }):find()
end

return {
  custom_dynamic_workspace_symbols = lsp_custom_dynamic_workspace_symbols,
}
