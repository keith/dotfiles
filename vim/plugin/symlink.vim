function! s:Resolve()
  let path = expand("%:p")
  let resolved = expand(resolve(system('readlink "' . path . '"')))

  " Required to re-resolve the link as opposed to reopening the same file again
  " since vim considers them to be able the same thing as one is a symlink for
  " the other.
  bwipeout
  execute "edit " . resolved
endfunction

command! Resolve call s:Resolve()
