function! neomake#makers#ft#bzl#EnabledMakers() abort
  return ['buildifier']
endfunction

function! neomake#makers#ft#bzl#buildifier() abort
  let maker = {
        \ 'exe': 'buildifier',
        \ 'args': ['-lint=check', '-lint=warn'],
        \ 'errorformat':
          \ '%E%f:%l: %m,' .
          \ '%W%f:%l: %m,' .
          \ '%Z%\s%#^~%#,' .
          \ '%-G%.%#',
      \ }
  return maker
endfunction
