" Concatenate the directory into the ls-files command
function! s:GitListCommand(directory)
  " Until you can use --recurse-submodules and --others together
  if filereadable(".gitmodules")
    return "git ls-files " . a:directory . " --cached --exclude-standard --recurse-submodules 2>/dev/null"
  else
    return "git ls-files " . a:directory . " --cached --exclude-standard --others 2>/dev/null"
  endif
endfunction

" Command for searching folders even if they
" aren't tracked with git
function! s:SearchCommand()
  if isdirectory(".git") || filereadable(".git")
    let l:command = s:GitListCommand(".")
  else
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

function! FuzzyFindCommand(vimCommand) abort
  let l:callback = {
        \ 'filename': tempname(),
        \ 'vimCommand':  a:vimCommand,
        \ 'window_id': win_getid(),
      \ }

  function! l:callback.on_exit(job_id, data, event) abort
    bdelete!
    call win_gotoid(self.window_id)
    if filereadable(self.filename)
      try
        let l:selected_filename = readfile(self.filename)[0]
        exec self.vimCommand . ' ' . l:selected_filename
      catch /E684/
      endtry
    endif
    call delete(self.filename)
  endfunction

  botright 10 new
  let l:term_command = '(' . s:SearchCommand() . ') | fzy > ' .  l:callback.filename
  silent call termopen(l:term_command, l:callback)
  setlocal nonumber norelativenumber
  startinsert
endfunction

nnoremap <C-p>  :call FuzzyFindCommand("edit")<cr>
nnoremap <C-p>e :call FuzzyFindCommand("edit")<cr>
nnoremap <C-p>v :call FuzzyFindCommand("vsplit")<cr>
nnoremap <C-p>s :call FuzzyFindCommand("split")<cr>
