" Concatenate the directory into the ls-files command
function! s:GitListCommand(directory)
  return "git ls-files " . a:directory . " --cached --exclude-standard --others"
endfunction

" Command for searching folders even if they
" aren't tracked with git
function! s:SearchCommand()
  let l:command = ""
  if isdirectory(".git") || filereadable(".git")
    let l:command = s:GitListCommand(".")
  endif

  if strlen(l:command) < 1
    let l:output = system("git rev-parse --show-toplevel")
    if v:shell_error == 0
      let l:output = substitute(l:output, "\\n", "", "")
      let l:command = s:GitListCommand(l:output)
    else
      let l:command = "find * -type f -o -type l"
    endif
  endif

  return l:command
endfunction

function! s:EscapeFilePath(path)
  return substitute(a:path, ' ', '\\ ', 'g')
endfunction

function! FuzzyFindCommand(vimCommand)
  update

  try
    let selection = system(s:SearchCommand() . " | fzy")
  catch /Vim:Interrupt/
    redraw!
    return
  endtry

  redraw!
  " Catch the ^C so that the redraw happens
  if v:shell_error
    return
  endif

  exec ":" . a:vimCommand . " " . s:EscapeFilePath(selection)
endfunction

nnoremap <C-p>  :call FuzzyFindCommand("edit")<cr>
nnoremap <C-p>e :call FuzzyFindCommand("edit")<cr>
nnoremap <C-p>v :call FuzzyFindCommand("vsplit")<cr>
nnoremap <C-p>s :call FuzzyFindCommand("split")<cr>
