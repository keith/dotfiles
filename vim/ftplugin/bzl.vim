setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

let g:formatdef_buildifierbuild = "'buildifier -type build -lint=fix -warnings=-load'"
let g:formatdef_buildifierbzl = "'buildifier -type bzl -lint=fix -warnings=-load'"
let g:formatdef_buildifiermodule = "'buildifier -type module -lint=fix -warnings=-load'"
let g:formatdef_buildifierworkspace = "'buildifier -type workspace -lint=fix -warnings=-load'"

function! WrapLabels() range
  silent! execute a:firstline .. "," .. a:lastline .. 's/^\( *\)\(.*\)$/\1"\2",/'
endfunction

noremap <leader>1 :call WrapLabels()<CR>

function! s:PrintError(message) abort
  echohl ErrorMsg
  echomsg a:message
  echohl None
endfunction

function! s:UpdateBzlmodDep() abort
  let l:line = getline('.')
  let l:arg_pattern = '\(^\|[(,]\)\s*%s\s*=\s*"\zs[^"]\+\ze"'
  if l:line !~# '\<bazel_dep\s*('
    call s:PrintError("Couldn't find bazel_dep on this line")
    return
  endif

  let l:name = matchstr(l:line, printf(l:arg_pattern, 'name'))
  if empty(l:name)
    call s:PrintError("Couldn't find bazel_dep name on this line")
    return
  endif

  if empty(matchstr(l:line, printf(l:arg_pattern, 'version')))
    call s:PrintError("Couldn't find bazel_dep version on this line")
    return
  endif

  let l:output = systemlist('newest-bzlmod ' . shellescape(l:name) . ' 2>/dev/null')
  if v:shell_error != 0
    call s:PrintError('newest-bzlmod failed for ' . l:name)
    return
  endif
  if empty(l:output)
    call s:PrintError('newest-bzlmod produced no output for ' . l:name)
    return
  endif

  let l:version = matchstr(l:output[0], printf(l:arg_pattern, 'version'))
  if empty(l:version)
    call s:PrintError('newest-bzlmod produced no version for ' . l:name)
    return
  endif

  let l:version_pattern = '\(^\|[(,]\)\(\s*version\s*=\s*"\)[^"]\+\("\)'
  let l:updated = substitute(l:line, l:version_pattern, '\1\2' . escape(l:version, '\&') . '\3', '')
  call setline('.', l:updated)
endfunction

if expand('%:t') ==# 'MODULE.bazel' || expand('%:t') =~# '\.MODULE\.bazel$'
  nnoremap <buffer> <silent> U :call <SID>UpdateBzlmodDep()<CR>
endif
