function description()
  return "Pinboard bookmarking"
end

function run()
  local window_index = focused_window_index()
  local webview_index = focused_webview_index(window_index)
  if #arguments > 0 then
    local command = arguments[1]
    table.remove(arguments, 1)
    if command == "same" then -- save to bookmarks in same page
      script = "javascript:if(document.getSelection){s=document.getSelection();}else{s='';};document.location='https://pinboard.in/add?next=same&url='+encodeURIComponent(location.href)+'&description='+encodeURIComponent(s)+'&title='+encodeURIComponent(document.title)"
      run_javascript(window_index, webview_index, script)
    elseif command == "rl" then -- read later
      script = "javascript:q=location.href;p=document.title;void(t=open('https://pinboard.in/add?later=yes&noui=yes&jump=close&url='+encodeURIComponent(q)+'&title='+encodeURIComponent(p),'Pinboard','toolbar=no,width=100,height=100'));t.blur();"
      run_javascript(window_index, webview_index, script)
    elseif command == "oldest" then -- view oldest unread item
      load_uri(window_index, webview_index, "https://pinboard.in/oldest/")
    elseif command == "random" then -- view random unread item
      load_uri(window_index, webview_index, "https://pinboard.in/random/?type=unread")
    else
      return false
    end
  else
    load_uri(window_index, webview_index, "https://pinboard.in/")
  end
  return true
end

function complete_command()
  if #arguments == 0 then
    return "oldest,rl,same,random"
  elseif #arguments == 1 then
    for i, item in ipairs({"oldest","rl","same","random"}) do
      if #item >= #arguments[1] then
        if string.sub(item, 1, #arguments[1]) == arguments[1] then
          return item
        end
      end
    end
  end
  return ""
end
