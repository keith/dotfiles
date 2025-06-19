setlocal completeopt+=preview
setlocal spell
setlocal textwidth=72

function! CopybaraPublic()
  let firstHash = 1
  while firstHash <= line('$') && getline(firstHash) !~ '^#'
    let firstHash += 1
  endwhile

  " If the commit message only has a subject, add 2 blank lines below
  if firstHash < 3
    call append(1, '')
    call append(2, '')
  endif

  " If the commit message already has content, make sure to insert the marker
  " and subject above the existing description
  call cursor(3, 1)
  call append(line('.') - 1, 'BEGIN_PUBLIC')
  call append(line('.') - 1, getline(1))
  call append(line('.') - 1, '')

  " Jump to after the commit message, leaving space if one isn't written, but
  " above the first comment line and insert the trailing marker
  let belowMessage = line('.')
  let nextHash = belowMessage - 1
  while nextHash <= line('$') && getline(nextHash) !~ '^#'
    let nextHash += 1
  endwhile

  let lastNonBlank = nextHash - 1
  while lastNonBlank > belowMessage && getline(lastNonBlank) =~ '^\s*$'
    let lastNonBlank -= 1
  endwhile

  call append(lastNonBlank, 'END_PUBLIC')
  " Jump to insert mode at the last line of the description
  call cursor(lastNonBlank, 1)
  call feedkeys("A", 'n')
endfunction

nnoremap <silent> <leader>s :call CopybaraPublic()<CR>
