function description()
  return "Coerces HTTP URLs to HTTPS"
end

-- Redirects from HTTP to HTTPS, except where excluded in the configuration
-- option `enforce-https.ignored-hosts`
function on_request_uri()
  local target = requested_uri
  local ignored_hosts = lookup_strings(config_file_path, "enforce-https.ignored-hosts")
  local host = string.gmatch(string.match(requested_uri, "://(.*)"), "[^/]+")()
  for _, ignored_host in ipairs(ignored_hosts) do
    if ignored_host == host then
      return
    end
  end
  if string.find(target, "http://") then
    target = target:gsub("http://", "https://")
    log_info("Redirecting to HTTPS")
    load_uri(window_index, webview_index, target)
  end
end
