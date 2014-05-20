if exists("g:mkdir_loaded") && g:mkdir_loaded
  finish
endif
let g:mkdir_loaded = 1

autocmd BufWritePre * call mkdir#Mkdir()
