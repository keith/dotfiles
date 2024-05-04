function! FuzzyFindCommand(vimCommand) abort
  let l:callback = {
        \ 'filename': tempname(),
        \ 'vimCommand':  a:vimCommand,
        \ 'window_id': win_getid(),
      \ }

  function! l:callback.on_exit(job_id, data, event) abort
    bdelete!
    call win_gotoid(self.window_id)
    if filereadable(self.filename)
      try
        let l:selected_filename = readfile(self.filename)[0]
        exec self.vimCommand . ' ' . l:selected_filename
      catch /E684/
      endtry
    endif
    call delete(self.filename)
  endfunction

  botright 10 new
  let l:term_command = 'find-files-async | fzf > ' .  l:callback.filename
  silent call termopen(l:term_command, l:callback)
  setlocal nonumber norelativenumber
  startinsert
endfunction

nnoremap <C-p>  :call FuzzyFindCommand("edit")<cr>
nnoremap <C-p>e :call FuzzyFindCommand("edit")<cr>
nnoremap <C-p>v :call FuzzyFindCommand("vsplit")<cr>
nnoremap <C-p>s :call FuzzyFindCommand("split")<cr>

nnoremap <C-p>w :call FuzzySymbols()<cr>
nnoremap <C-p>r :call FuzzyReferences()<cr>

function! FuzzySymbols() abort
  lua require('workspace_symbols').lsp_dynamic_workspace_symbols({})
endfunction

function! FuzzyReferences() abort
  lua require('telescope.builtin').lsp_references({cwd = '.'})
endfunction
