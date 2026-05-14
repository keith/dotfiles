local M = {}

local header_extensions = {
  "h",
  "hh",
  "hpp",
  "hxx",
}

local source_extensions = {
  "c",
  "cc",
  "cpp",
  "cxx",
  "m",
  "mm",
}

local function extension(path)
  return vim.fn.fnamemodify(path, ":e")
end

local function edit_file(path)
  vim.cmd.edit(vim.fn.fnameescape(vim.fn.fnamemodify(path, ":.")))
end

-- copied from upstream clangd.lua to silence output
local function clangd_alternate()
  local bufnr = vim.api.nvim_get_current_buf()
  local clangd_client = vim.lsp.get_clients({ bufnr = bufnr, name = "clangd" })[1]
  if not clangd_client then
    return false
  end

  local params = { uri = vim.uri_from_bufnr(bufnr) }
  local response, err = clangd_client:request_sync("textDocument/switchSourceHeader", params, 1000, bufnr)
  if err or not response then
    return false
  elseif response.err then
    error(tostring(response.err))
    return false
  elseif not response.result then
    return false
  end

  edit_file(vim.uri_to_fname(response.result))
  return true
end

local function file_exists(path)
  return vim.fn.filereadable(path) ~= 0
end

local function alternate_extensions(ext)
  if vim.tbl_contains(header_extensions, ext) then
    return source_extensions
  elseif vim.tbl_contains(source_extensions, ext) then
    return header_extensions
  end

  return {}
end

local function swap_extensions(path)
  local ext = extension(path)
  if ext == "" then
    return {}
  end

  local base = vim.fn.fnamemodify(path, ":r")
  local paths = {}
  for _, alternate_ext in ipairs(alternate_extensions(ext)) do
    table.insert(paths, base .. "." .. alternate_ext)
  end
  return paths
end

local function add_swapped_paths(paths, path)
  for _, alternate_path in ipairs(swap_extensions(path)) do
    table.insert(paths, alternate_path)
  end
end

local function edit_first_existing(paths)
  for _, path in ipairs(paths) do
    if file_exists(path) then
      edit_file(path)
      return true
    end
  end

  return false
end

function M.alternate()
  if clangd_alternate() then
    return
  end

  local file = vim.fn.expand("%")
  local parts = vim.split(file, "/")
  local paths = {}

  -- llvm/lib/ADT/SmallVector.cpp -> llvm/lib/ADT/SmallVector.h
  add_swapped_paths(paths, file)

  if parts[2] == "lib" then
    -- llvm/lib/ADT/SmallVector.cpp -> llvm/include/llvm/ADT/SmallVector.h
    local new_parts = {parts[1], "include", parts[1]}
    for i = 3, #parts do
      table.insert(new_parts, parts[i])
    end
    add_swapped_paths(paths, table.concat(new_parts, "/"))
  elseif parts[2] == "include" then
    -- llvm/include/llvm/ADT/SmallVector.h -> llvm/lib/ADT/SmallVector.cpp
    local new_parts = {parts[1], "lib"}
    for i = 4, #parts do
      table.insert(new_parts, parts[i])
    end
    add_swapped_paths(paths, table.concat(new_parts, "/"))
  end

  if edit_first_existing(paths) then
    return
  end

  vim.api.nvim_err_writeln("no alternate file found, tried: " .. table.concat(paths, ", "))
end

function M.setup()
  vim.api.nvim_command("command! A lua require('keith.alternate').alternate()")
end

return M
