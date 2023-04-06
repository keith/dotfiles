function! s:bzl_includeexpr(fname) abort
  let l:parts = split(a:fname, '//')
  let l:path = l:parts[-1]
  let l:parts = split(l:path, ':')
  let l:last_component = remove(l:parts, -1)
  if l:last_component =~# '\.bzl$'
    return join(l:parts + [l:last_component], '/')
  else
    let l:path = join(l:parts + ['BUILD.bazel'], '/')
    if file_readable(l:path)
      return l:path
    else
      return join(l:parts + ['BUILD'], '/')
  endif
endfunction

set isfname+=:
let &includeexpr=expand('<SID>') .. 'bzl_includeexpr(v:fname)'

function! s:yankBazelTarget(word) abort
  if a:word =~# ':'
    let l:path = substitute(a:word, '^/\+', '', '')
  else
    let l:fname = expand('%:h')
    let l:path = l:fname . ':' . a:word
  endif
  let cmd = printf("echo -n '%s' | pbcopy", l:path)
  call system(cmd)
endfunction

nnoremap <silent> yit :call <SID>yankBazelTarget(expand('<cfile>'))<CR>
