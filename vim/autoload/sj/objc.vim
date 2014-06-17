function! sj#objc#JoinIfClause()
  if sj#SearchUnderCursor('if\s*([^)]*)\s*{\?') <=  0
    return 0
  endif

  call sj#PushCursor()
  normal! JJf}xxF{xx
  " Using vim-surround
  " normal JJds{
  call sj#PopCursor()

  return 1
endfunction

function! sj#objc#SplitIfClause()
  let pattern = '\v^\s*(if\s*\([^\)]*\)\s*)(return.*)$'
  if sj#SearchUnderCursor(pattern) <= 0
    return 0
  endif

  call sj#PushCursor()
  call sj#ReplaceMotion('V', substitute(getline('.'), pattern, '\1{\n\2\n}', ''))
  call sj#PopCursor()

  return 1
endfunction
