function! s:Resolve()
  let path = expand("%:p")
  let link = system('readlink "' . path . '"')
  " If the file isn't a symlink, let the expand logic happen still
  if empty(link)
    let link = path
  endif

  let resolved = expand(resolve(link))
  let modified = fnamemodify(resolved, ":~:.")

  " If the files are the same as what is open, bail
  if modified == expand("%")
    return
  endif

  " Required to re-resolve the link as opposed to reopening the same file again
  " since vim considers them to be able the same thing as one is a symlink for
  " the other.
  bwipeout
  execute "edit " . modified
endfunction

command! Resolve call s:Resolve()
