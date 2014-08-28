augroup alternate_headers
  autocmd!
  autocmd FileType objc,c,cpp command! -buffer A :call alternate#Alternate("edit")
  autocmd FileType objc,c,cpp command! -buffer AS :call alternate#Alternate("split")
  autocmd FileType objc,c,cpp command! -buffer AV :call alternate#Alternate("vsplit")
  autocmd FileType objc,c,cpp command! -buffer AT :call alternate#Alternate("tabnew")
augroup END
