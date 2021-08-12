function! s:PrintNeomakeResult()
  echom printf('%s exited with %d',
        \ g:neomake_hook_context.jobinfo.maker.name,
        \ g:neomake_hook_context.jobinfo.exit_code)
endfunction

augroup neomake_config
  autocmd!
  autocmd! BufWritePost * Neomake

  autocmd User NeomakeJobFinished call s:PrintNeomakeResult()
augroup END

let g:neomake_error_sign = {'text': '>>', 'texthl': 'NeomakeErrorSign'}
let g:neomake_warning_sign = {'text': '>>', 'texthl': 'NeomakeWarningSign' }
let g:neomake_message_sign = {'text': 'm>', 'texthl': 'NeomakeMessageSign' }
let g:neomake_info_sign = {'text': 'i>', 'texthl': 'NeomakeInfoSign'}

let g:neomake_python_enabled_makers = []
let g:neomake_c_enabled_makers   = []
let g:neomake_cpp_enabled_makers = []
let g:neomake_ruby_enabled_makers = []
let g:neomake_java_enabled_makers = []
let g:neomake_rust_enabled_makers = []
let g:neomake_swift_enabled_makers = ["swiftc"]
let g:neomake_open_list = 2
let g:neomake_verbose = 0
