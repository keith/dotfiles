setlocal shiftwidth=4
setlocal spell
setlocal tabstop=4
setlocal textwidth=110

function! CopyTestCommand()
  let matches = matchlist(getline('.'),'^\s*func \(test\w\+\)()')
  if empty(matches)
    echom "No test on current line"
    return
  endif

  let class_pattern = 'class\s\+\(\w\+\)\s*:\s*XCTestCase\>'
  let class_line = search(class_pattern, 'bWn')
  if class_line == 0
    echom "Couldn't find class"
    return
  endif

  let class_name_match = matchlist(getline(class_line), class_pattern)
  if empty(class_name_match)
    echom "Couldn't extract class name from: " . getline(class_line)
    return
  endif

  let parent = expand("%:h")
  let module_match = matchlist(parent, '\<Tests/\(\w\+\)\>')
  if empty(module_match)
    echom "Couldn't find module in directory structure"
    return
  endif

  let command = "swift test --filter " . module_match[1] . "." . class_name_match[1] . "/" . matches[1] . " 2>&1 | xcpretty -t"
  call setreg("+", command)
endfunction

nnoremap <leader>c :call CopyTestCommand()<CR>
nnoremap <leader>t :Neomake swiftpmtest<CR>
nnoremap <leader>f :TestNearest -strategy=neomake<CR>
