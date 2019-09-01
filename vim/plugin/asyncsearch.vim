if filereadable(".git") || isdirectory(".git")
  let s:executable="git"
  let s:arguments="grep --recurse-submodules --line-number {}"
else
  let s:executable="ag"
  let s:arguments="--vimgrep --case-sensitive {}"
endif

command -nargs=+ -complete=file Grep call <SID>Grep(<q-args>)
function! s:Grep(args)
  if empty(a:args)
    echohl ErrorMsg
    echo 'Search is empty, skipping'
    echohl None

    return
  endif

  cclose
  let l:formatted_args = '"' . escape(a:args, '\') . '"'
  let l:arguments = substitute(s:arguments, '{}', l:formatted_args, '')
  let l:maker = {
        \ 'exe': s:executable,
        \ 'args': l:arguments,
        \ 'errorformat': &errorformat,
      \ }
  call neomake#Make(0, [l:maker])
endfunction

function! s:Escape(string)
  let l:string = a:string
  let l:string = escape(escape(l:string, '"'), '"')
  let l:string = escape(escape(l:string, '('), '(')
  let l:string = escape(escape(l:string, ')'), ')')
  return l:string
endfunction

nnoremap <silent> s :set operatorfunc=<SID>GrepMotion<CR>g@
function! s:GrepMotion(...) abort
  call highlightedyank#off()
  let save = @@
  silent execute "normal! `[v`]y"
  call s:Grep(s:Escape(@@))
  let @@ = save
  call highlightedyank#on()
endfunction

vnoremap <silent> s :<C-U>call <SID>GrepVisual()<CR>
function! s:GrepVisual()
  call s:Grep(s:Escape(s:GetVisualSelection()))
endfunction

" What a PITA https://stackoverflow.com/a/6271254/902968
function! s:GetVisualSelection()
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  let lines = getline(line_start, line_end)

  if len(lines) == 0
    return ''
  endif

  if len(lines) > 1
    throw 'Cannot search for multiline expressions'
    return ''
  endif

  let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][column_start - 1:]
  return join(lines, "\n")
endfunction
