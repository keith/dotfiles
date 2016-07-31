function description()
  return "Loads custom CSS and JS"
end

-- Loads a CSS or JS file into the current document
function run()
  log_info("testing loging")
  if #arguments > 0 then
    local file_name = arguments[1]
    log_info("loading " .. file_name)
    local window_index = focused_window_index()
    local webview_index = focused_webview_index(window_index)
    if string.ends_with(file_name, ".css") then
      return load_css(file_name, window_index, webview_index)
    elseif string.ends_with(file_name, ".js") then
      return load_js(file_name, window_index, webview_index)
    end
  end
  return false
end

-- Loads all CSS and JS files from the configuration option
-- `user-content.default-paths` into every page, as well as the files from
-- `user-content.site-paths` based on the domain name of the page.
function on_load_uri()
  load_default_files()
  load_site_files()
end

function load_site_files()
  local host = string.gmatch(string.match(requested_uri, "://(.*)"), "[^/]+")()
  local base_paths = lookup_strings(config_file_path, "user-content.site-paths")
  for _, base_path in ipairs(base_paths) do
    load_css(string.format("%s/%s.css", base_path, host), window_index, webview_index)
    load_js(string.format("%s/%s.js", base_path, host), window_index, webview_index)
  end
end

function load_default_files()
  local default_paths = lookup_strings(config_file_path, "user-content.default-paths")
  for _, base_path in ipairs(default_paths) do
    for _, file_name in ipairs(list_files(base_path)) do
      if string.ends_with(file_name, ".css") then
        load_css(string.format("%s/%s", base_path, file_name), window_index, webview_index)
      elseif string.ends_with(file_name, ".js") then
        load_js(string.format("%s/%s.js", base_path, file_name), window_index, webview_index)
      end
    end
  end
end

function load_css(path, window_index, webview_index)
  local css = load_file(path)
  if css then
    log_info(string.format("Loading CSS: %s", path))
    add_styles(window_index, webview_index, css)
    return true
  end
  return false
end

function load_js(path, window_index, webview_index)
  local js = load_file(path)
  if js then
    log_info(string.format("Loading JS: %s", path))
    run_javascript(window_index, webview_index, js)
    return true
  end
  return false
end

function load_file(file_path)
  local file = io.open(file_path)
  if not file then return nil end
  local content = file:read("*a")
  file:close()
  return content
end

function list_files(directory)
  local index, files = 0, {}
  local list_handle = io.popen('ls "' .. directory .. '"')
  for filename in list_handle:lines() do
    index = index + 1
    files[index] = filename
  end
  list_handle:close()
  return files
end

function string.ends_with(text, ending)
  return ending == '' or string.sub(text, -string.len(ending)) == ending
end
