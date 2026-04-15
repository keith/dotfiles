local function read_plugin_specs(path)
  local specs = {}
  local f = assert(io.open(path, "r"))
  for line in f:lines() do
    if line ~= "" then
      table.insert(specs, { src = "https://github.com/" .. line })
    end
  end
  f:close()
  return specs
end

local specs = {}
for _, file in ipairs { "~/.vim/plugins", "~/.vim/nvimplugins" } do
  vim.list_extend(specs, read_plugin_specs(vim.fn.expand(file)))
end
vim.pack.add(specs)
