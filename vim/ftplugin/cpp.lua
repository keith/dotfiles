local util = require 'lspconfig.util'

-- copied from upstream clangd.lua to silence output
local function clangd_alternate()
  local bufnr = util.validate_bufnr(0)
  local clangd_client = util.get_active_client_by_name(bufnr, 'clangd')
  if clangd_client then
    local params = { uri = vim.uri_from_bufnr(bufnr) }
    clangd_client.request('textDocument/switchSourceHeader', params, function(err, result)
      if err then
        error(tostring(err))
        return false
      elseif not result then
        return false
      end

      vim.api.nvim_command('edit ' .. vim.uri_to_fname(result))
      return true
    end, bufnr)
  else
    return false
  end
end

local function file_exists(path)
  return vim.fn.filereadable(path) ~= 0
end

local function swap_extension(path)
  if string.match(path, ".h$") then
    return string.gsub(path, ".h$", ".cpp")
  else
    return string.gsub(path, ".cpp$", ".h")
  end
end

function Alternate()
  if clangd_alternate() then
    return
  end

  local file = vim.fn.expand("%")
  local parts = vim.split(file, "/")
  local new_file = ""

  if parts[2] == "lib" then
    -- Either option
    -- llvm/lib/ADT/SmallVector.cpp -> llvm/lib/ADT/SmallVector.h
    -- llvm/lib/ADT/SmallVector.cpp -> llvm/include/llvm/ADT/SmallVector.h

    new_file = swap_extension(file)
    if file_exists(new_file) then
      vim.cmd.edit(new_file)
      return
    end

    local new_parts = {parts[1], "include", parts[1]}
    for i = 3, #parts do
      table.insert(new_parts, parts[i])
    end
    new_file = swap_extension(table.concat(new_parts, "/"))
  elseif parts[2] == "include" then
    -- llvm/include/llvm/ADT/SmallVector.h -> llvm/lib/ADT/SmallVector.cpp

    local new_parts = {parts[1], "lib"}
    for i = 4, #parts do
      table.insert(new_parts, parts[i])
    end

    new_file = swap_extension(table.concat(new_parts, "/"))
  else
    vim.api.nvim_err_writeln("invalid path for alternate: " .. file)
    return
  end

  if file_exists(new_file) then
    vim.cmd.edit(new_file)
  else
    vim.api.nvim_err_writeln("no alternate file found, tried: " .. new_file)
  end
end

vim.api.nvim_command("command! A call luaeval('Alternate()')")
