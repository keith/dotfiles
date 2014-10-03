" File: tslime.vim
" Author: Keith Smiley
" Description: tslime. Simplified
" Last Modified: October 03, 2014

if exists("g:loaded_tslime") && g:loaded_tslime
  finish
endif
let g:loaded_tslime = 1

if !exists("g:tslime_normal_map")
  let g:tslime_normal_map = "\<C-c><C-c>"
endif

if !exists("g:tslime_visual_map")
  let g:tslime_visual_map = "\<C-c><C-c>"
endif

function! s:SetTmuxBuffer(text)
  call system("tmux set-buffer -- '" . substitute(a:text, "'", "'\\\\''", 'g') . "'")
endfunction

function! s:SetPaneNumber()
  call system("tmux display-panes")
  let pane = input("Pane number: ")

  if empty(pane)
    throw "Pane number required for use"
  endif

  let g:tslime_pane_number = pane
endfunction

function! SendToTmux(text)
  if !exists("g:tslime_pane_number")
    try
      call s:SetPaneNumber()
    catch /.*/
      echohl WarningMsg |
        \ echomsg v:exception |
        \ echohl None
      return
    endtry
  endif

  for line in split(a:text, '\n\zs' )
    call <SID>SetTmuxBuffer(line)
    call system("tmux paste-buffer -t " . g:tslime_pane_number)
    sleep 2m
  endfor
endfunction

execute 'nnoremap ' . g:tslime_normal_map . ' vip"ry:call SendToTmux(@r)<CR>'
execute 'vnoremap ' . g:tslime_visual_map . ' "ry:call SendToTmux(@r)<CR>'
