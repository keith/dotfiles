function! s:bzl_includeexpr(fname) abort
  let l:fname = a:fname

  if l:fname =~# '__BAZEL_XCODE_SDKROOT__'
    if !exists('s:xcode_sdk_root')
      let s:xcode_sdk_root = trim(system('xcrun --show-sdk-path'))
    endif
    let l:fname = substitute(l:fname, '__BAZEL_XCODE_SDKROOT__', '\=s:xcode_sdk_root', 'g')
  endif
  if l:fname =~# '__BAZEL_XCODE_DEVELOPER_DIR__'
    if !exists('s:xcode_developer_dir')
      let s:xcode_developer_dir = trim(system('xcode-select -p'))
    endif
    let l:fname = substitute(l:fname, '__BAZEL_XCODE_DEVELOPER_DIR__', '\=s:xcode_developer_dir', 'g')
  endif

  let l:parts = split(l:fname, '//')
  let l:path = l:parts[-1]
  let l:parts = split(l:path, ':')
  let l:last_component = remove(l:parts, -1)
  let l:file_path = join(l:parts + [l:last_component], '/')
  if l:last_component =~# '\.bzl$' || file_readable(l:file_path)
    return l:file_path
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
