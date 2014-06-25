function! sj#objc#JoinIfClause()
  let pattern = '\v^\s*\w+\s*\(([^\)]*)\)\s*\{?'
  if sj#SearchUnderCursor(pattern) <=  0
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
  let pattern = '\v^\s*(\w+\s*\([^\)]*\)\s*)(.*)$'
  if sj#SearchUnderCursor(pattern) <= 0
    return 0
  endif

  call sj#PushCursor()
  call sj#ReplaceMotion('V', substitute(getline('.'), pattern, '\1{\n\2\n}', ''))
  call sj#PopCursor()

  return 1
endfunction

function! sj#objc#JoinSetProperty()
  let pattern = '\v^.*\.\w+\s*\=.*$'
  if sj#SearchUnderCursor(pattern) <= 0
    return 0
  endif

  call sj#PushCursor()
  normal! 0f.w~f;x
  call sj#ReplaceMotion('V', substitute(getline('.'), '\v\.(\w+)\s*\=\s*', ' set\1:', ''))
  call sj#ReplaceMotion('V', substitute(getline('.'), '\v^(.*)$', '[\1];', ''))
  call sj#PopCursor()

  return 1
endfunction

function! sj#objc#SplitSetProperty()
  let pattern = '\v^.*set\w+:.*$'
  if sj#SearchUnderCursor(pattern) <= 0
    return 0
  endif

  call sj#PushCursor()
  normal! ^wwXxxr.l~
  call sj#ReplaceMotion('V', substitute(getline('.'), '\v:', ' = ', ''))
  normal! f]xF[x
  call sj#PopCursor()

  return 1
endfunction

function! sj#objc#JoinNSNumber()
  let pattern = '\v^(\s*.*)\[NSNumber\snumberWith\w+:([^\]]*)\](.*)$'
  if sj#SearchUnderCursor(pattern) <= 0
    return 0
  endif

  call sj#PushCursor()
  call sj#ReplaceMotion('V', substitute(getline('.'), pattern, '\1@(\2)\3', ''))
  call sj#PopCursor()

  return 1
endfunction
