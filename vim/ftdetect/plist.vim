function! s:ConvertPlistToXML()
  let converted = get(b:, "converted_to_xml", 0)
  if converted
    return
  endif

  if !executable("plutil")
    return
  endif

  call system('plutil -convert xml1 "' . expand("<afile>:p") . '"')
  let b:converted_to_xml = 1
endfunction

autocmd BufReadPre *.plist call s:ConvertPlistToXML()
