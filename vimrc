"
" Keith Smiley's (http://keith.so) vimrc, do with what you will
"
"

" This must be first, because it changes other options
if &compatible
  set nocompatible
endif

" Source files before plugins
source ~/.vim/before/*.vim

" Plugin setup
filetype off

set packpath^=~/.vimplugins/bundle
if has('nvim')
  set packpath^=~/.vimplugins/nvimbundle
endif

filetype plugin indent on " Re-enable after setup
syntax enable " Enable vim syntax highlighting as is (enable != on)

" Use space as leader!
let g:mapleader="\<Space>"

" I - Disable the startup message
" a - Shorten some messsages to avoid pressing enter in some cases
" t - Truncate if the message is too wide instead of making you 'hit enter to continue'
set shortmess=Iat

set shell=$SHELL               " Set the default shell
set encoding=utf-8             " Set the default encodings just in case $LANG isn't set
set autoindent                 " Indent the next line matching the previous line
set smartindent                " Smart auto-indent when creating a new line
set tabstop=2                  " Number of spaces each tab counts for
set shiftwidth=2               " The space << and >> moves the lines
set softtabstop=2              " Number of spaces for some tab operations
set shiftround                 " Round << and >> to multiples of shiftwidth
set expandtab                  " Insert spaces instead of actual tabs
set smarttab                   " Delete entire shiftwidth of tabs when they're inserted
set history=1000               " The number of history items to remember
set backspace=indent,eol,start " Backspace settings
set nostartofline              " Keep cursor in the same place after saves
set showcmd                    " Show command information on the right side of the command line
set isfname-==                 " Remove characters from filenames for gf

" Create a directory if it doesn't exist yet
function! s:EnsureDirectory(directory)
  if !isdirectory(expand(a:directory))
    call mkdir(expand(a:directory), 'p')
  endif
endfunction

let s:subdir = "vim"
if has('nvim')
  let s:subdir = "nvim"
endif

" https://stackoverflow.com/a/15317146/902968
" Save backup files, storage is cheap, losing changes is sad
set backup
let &backupdir=$HOME . '/.tmp/' . s:subdir . '/backup//'
call s:EnsureDirectory(&backupdir)

" Write undo tree to a file to resume from next time the file is opened
if has('persistent_undo')
  set undolevels=2000            " The number of undo items to remember
  set undofile                   " Save undo history to files locally
  let &undodir=$HOME . '/.tmp/' . s:subdir . '/undo//'
  call s:EnsureDirectory(&undodir)
endif

let &directory=$HOME . '/.tmp/' . s:subdir . '/swap//'
call s:EnsureDirectory(&directory)

let &viewdir=$HOME . '/.tmp/' . s:subdir . '/view'
call s:EnsureDirectory(&viewdir)

" On quit reset title
let &titleold=getcwd()

let &background=system("get-appearance")
silent! colorscheme solarized

set ttyfast                 " Set that we have a fast terminal
set t_Co=256                " Explicitly tell Vim that the terminal supports 256 colors
set lazyredraw              " Don't redraw vim in all situations
set synmaxcol=500           " The max number of columns to try and highlight
set noerrorbells            " Don't make noise
set autoread                " Watch for file changes and auto update
set showmatch               " Set show matching parenthesis
set matchtime=2             " The amount of time matches flash
set display=lastline        " Display super long wrapped lines
set number                  " Shows line numbers
set nrformats-=octal        " Never use octal notation
set nrformats+=unsigned     " Ignore '-' denoting negative numbers
set nojoinspaces            " Don't add 2 spaces when using J
set mouse=a                 " Enable using the mouse if terminal emulator
set mousehide               " Hide the mouse on typing
set hlsearch                " Highlight search terms
set incsearch               " Show searches as you type
set wrap                    " Softwrap text
set linebreak               " Don't wrap in the middle of words
set ignorecase              " Ignore case when searching
set smartcase               " Ignore case if search is lowercase, otherwise case-sensitive
set title                   " Change the terminal's title
set updatetime=2000         " Set the time before plugins assume you're not typing
set scrolloff=5             " Lines the cursor is to the edge before scrolling
set sidescrolloff=5         " Same as scrolloff but horizontal
set gdefault                " Adds g at the end of substitutions by default
set virtualedit=block       " Allow the cursor to move off the side in visual block
set nofoldenable
set foldmethod=indent       " Decide where to fold based
set foldnestmax=5           " Set deepest fold to x levels
set exrc                    " Source local .vimrc files
set secure                  " Don't load autocmds from local .vimrc files
set tags^=.tags,.git/tags   " Add local .tags file
set signcolumn=yes

" https://kinbiko.com/vim/my-shiniest-vim-gems/
" Remove comments when joining lines with J
set formatoptions+=j

" Don't remove % on J
set comments-=:%

" Completion options
set complete=.,w,b,u,t,kspell
set completeopt=menuone,noselect
set wildmenu                                           " Better completion in the CLI
set wildmode=longest:full,full                         " Completion settings

" Ignore these folders for completions
set wildignore+=.hg,.git,.svn                          " Version control
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg         " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.pyc " compiled object files
set wildignore+=*.resolved                             " package manager lock files
set wildignore+=tags,.tags

" Dictionary for custom words
set dictionary+=/usr/share/dict/words
set spellfile=$HOME/.vim/custom-words.utf-8.add
set spelloptions=camel

" Set mapping and key timeouts
set timeout
set timeoutlen=1000
set ttimeoutlen=100

" Setting to indent wrapped lines
if exists('+breakindent')
  set breakindent
  set breakindentopt=shift:2
endif

" Check for file specific vim settings in the last 3 lines of the file
set modeline
set modelines=2

" Functions for status line config since these functions aren't loaded
" when the vimrc is sourced
function! CurrentTag(...)
  if &ft != 'objc' && exists('g:tagbar_iconchars')
    return call('tagbar#currenttag', a:000)
  else
    return ''
  endif
endfunction

" Status line setup (without plugins)
set laststatus=2 " Always show the statusline
" Left Side
set statusline=
set statusline+=%#IncSearch#%{&paste?'\ \ PASTE\ ':''}%*
set statusline+=\ %f
set statusline+=\ %m
set statusline+=\ %r
set statusline+=%=
" Right Side
" TODO: use treesitter here first if enabled? nvim_treesitter#statusline
set statusline+=%{CurrentTag('%s\ <\ ','','')}
set statusline+=%y
set statusline+=\ \ %P
set statusline+=-%l
set statusline+=-%c

if has('clipboard')     " If the feature is available
  set clipboard=unnamed " copy to the system clipboard

  if has('unnamedplus')
    set clipboard+=unnamedplus
  endif
endif

nnoremap Q :quit<CR>

" Easier save mapping
nnoremap W :write<CR>

" Disable K
vnoremap K <Nop>

" Reselect visual blocks after movement
vnoremap < <gv
vnoremap > >gv

" Don't copy the contents of an overwritten selection.
vnoremap p P

" Move as expected on wrapped lines
noremap j gj
noremap gj j
noremap <Down> gj
inoremap <Down> <C-o>gj
noremap k gk
noremap gk k
noremap <Up> gk
inoremap <Up> <C-o>gk

nnoremap <C-u> 10k
nnoremap <C-d> 10j

" https://www.reddit.com/r/vim/comments/4jy1mh/slightly_more_subltle_n_and_n_behavior/
" Keep search matches in the middle of the window unless the next match is in
" the same viewport
function! s:NextAndCenter(cmd)
  let l:view = winsaveview()
  try
    execute 'normal! ' . a:cmd
  catch /^Vim\%((\a\+)\)\=:E486/
    " Fake a 'rethrow' of an exception without causing a 3 line error message
    echohl ErrorMsg
    echo 'E486: Pattern not found: ' . @/
    echohl None
  endtry

  if l:view.topline != winsaveview().topline
    normal! zzzv
  endif
endfunction

nnoremap <silent> n :set hlsearch \| call <SID>NextAndCenter('n')<CR>
nnoremap <silent> N :set hlsearch \| call <SID>NextAndCenter('N')<CR>

" Remap capital y to act more like other capital letters
nnoremap Y y$

" Force root permission saves
cnoremap w!! w !sudo tee % >/dev/null

command! -bang Q q<bang>
command! -bang -nargs=* -complete=file W w<bang> <args>

" Change the way splits open by default
set splitbelow
set splitright

" Better movement
nnoremap H ^
vnoremap H ^

" Switch to the last file
nnoremap <leader><leader> <C-^>

" Move to last edit location and put it in the center of the screen
nnoremap <C-o> <C-o>zz

" Remove the last search thus clearing the highlight
" This clears the search register denoted by @/
nnoremap <silent> <leader>4 :let @/=""<CR>

" Don't automatically jump on search
nnoremap * :keepjumps normal! mi*`i<CR>
nnoremap # :keepjumps normal! mi#`i<CR>

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

augroup trailing_highlight
  autocmd!
  autocmd BufWinEnter * match trailing_whitespace /\s\+$/
  autocmd InsertEnter * match trailing_whitespace /\s\+\%#\@<!$/
  autocmd InsertLeave * match trailing_whitespace /\s\+$/
augroup END

hi! link trailing_whitespace ErrorMsg

" Close all the lists
nnoremap <silent> <leader>q :call <SID>CloseLists()<CR>
function! s:CloseLists()
  lclose
  cclose
  pclose
  silent! TagbarClose
endfunction

command! ClearWhitespace call s:ClearWhitespace()
function! s:ClearWhitespace()
  let l:line = line('.')
  let l:column = col('.')
  keepjumps silent! %s/\s\+$//e
  call cursor(l:line, l:column)
  call histdel('search', -1)
endfunction

command! FormatShellCommand call s:FormatShellCommand()
function! s:FormatShellCommand()
  let l:line = line('.')
  let l:column = col('.')
  let l:content = getline('.')
  let l:output = systemlist("print -l -- " . l:content)
  call setline(".", l:output)
  keepjumps silent! 1,$-1s/\n/ \\\r /e
  call cursor(l:line, l:column)
  call histdel('search', -1)
endfunction

" Fixup my screen
nnoremap U :syntax sync fromstart<CR>:redraw!<CR>

" Running as diff
if &diff
  set modifiable
  set noreadonly
  if tabpagenr('$') == 1
    nnoremap ZZ :wqall<CR>
  endif
endif

" Jump to next/previous merge conflict marker
nnoremap <silent> ]c /\v^(\<\|\=\|\\|\|\>){7}([^=].+)?$<CR>zz
nnoremap <silent> [c ?\v^(\<\|\=\|\\|\|\>){7}([^=].+)\?$<CR>zz

" Position resume
function! s:PositionRecall()
  if &filetype =~? 'gitcommit\|gitrebase'
    return
  endif

  if line("'\"") > 0 && line("'\"") <= line('$')
    execute "normal g`\"zz"
  endif
endfunction

function! s:SetColorColumn()
  if &textwidth == 0
    " Set colorcolumn specifically to 80 unless textwidth is set
    set colorcolumn=80
  else
    " Show a line past the text width
    set colorcolumn=+1
  endif
endfunction

" Window sizes
augroup window_sizes
  autocmd!

  " Set the minimum window width for vertical splits
  autocmd VimEnter * silent! set winwidth=80
  autocmd VimEnter * silent! set winminwidth=20
augroup END

augroup theme
  autocmd!
  autocmd FocusGained * silent! let &background=system("get-appearance")
augroup END

" Various filetype settings
augroup ft_settings
  autocmd!

  " Comment string settings
  if empty(&commentstring) | setlocal commentstring=#\ %s | endif
  autocmd FileType c,cpp,go,objc,php setlocal commentstring=//\ %s

  " Treat more extensions as .zip files
  autocmd BufReadCmd *.aar call zip#Browse(expand("<amatch>"))
  autocmd BufReadCmd *.apk call zip#Browse(expand("<amatch>"))
  autocmd BufReadCmd *.conda call zip#Browse(expand("<amatch>"))
  autocmd BufReadCmd *.ipa call zip#Browse(expand("<amatch>"))
  autocmd BufReadCmd *.whl call zip#Browse(expand("<amatch>"))

  " Save files on some focus lost events, like switching splits
  autocmd BufLeave,FocusLost * silent! wall

  " Don't auto insert a comment when using O/o for a newline
  autocmd VimEnter,BufRead,FileType * set formatoptions-=o
  " Break lines based on 'textwidth' even if the line was longer before
  " starting insert mode. This is useful for pasting long lines, and
  " then continuing at the end of them.
  autocmd VimEnter,BufRead,FileType * set formatoptions-=l

  " Set color column based on textwidth setting
  autocmd FileType * call s:SetColorColumn()

  " Return to the same position you left the file in
  autocmd BufRead * call s:PositionRecall()

  autocmd FocusGained,BufEnter,CursorHold <buffer> checktime

  " Create the binary spell file when opening vim
  autocmd VimEnter * execute 'silent mkspell! ' . &spellfile

  " Close location list when closing the window it belongs to
  autocmd QuitPre * nested if &filetype != 'qf' | silent! lclose | endif

  autocmd FileType fugitiveblame nnoremap <buffer> x :call system("git open " . expand("<cword>"))<CR>

  autocmd BufNewFile *.sh 0r ~/.vim/templates/skeleton.sh | normal! G
  autocmd BufNewFile *.sh let b:is_new = 1
  autocmd BufWritePost *.sh
      \  if get(b:, 'is_new', 0)
      \|   silent execute '!chmod +x %'
      \|   let b:is_new = 0
      \| endif
augroup END

" ObjC curly brace error fix
let g:c_no_curly_error = 1

" https://github.com/thoughtbot/dotfiles/pull/471
let g:is_posix = 1

command -nargs=? Gqf silent cexpr systemlist('git diff-qf <args>') | copen

command! -bang Delete
      \ let s:file = expand('%:p') |
      \ execute 'bdelete!' |
      \ if delete(s:file) != 0 |
      \   echoerr 'Failed to delete "' . s:file . '"' |
      \ endif |
      \ unlet s:file

command! -nargs=* Chmod
      \ let s:file = expand('%:p') |
      \ call system('chmod +x ' . shellescape(s:file)) |
      \ if v:shell_error |
      \   echoerr 'Failed to chmod +x "' . s:file . '"' |
      \ endif |
      \ unlet s:file

command! Helptags helptags ALL

cnoremap DIR <c-r>=expand('%:p:h') . '/'<CR>
command! CDC cd %:p:h

function! s:EditBuildFile() abort
  let s:start = '%:p:h'

  " If you're already editing a BUILD file, jump to the next one up the tree
  let s:current = expand('%:t')
  if s:current == "BUILD" || s:current == "BUILD.bazel"
    let s:start = '%:p:h:h'
  endif

  let s:dir = expand(s:start)
  let s:path = 'nothing'
  while v:true
    let s:path = s:dir . '/BUILD.bazel'
    if filereadable(s:path)
      break
    endif

    let s:path = s:dir . '/BUILD'
    if filereadable(s:path)
      break
    endif

    let s:dir = resolve(s:dir . '/..')
    if s:dir == "/"
      break
    endif
  endwhile

  if filereadable(s:path)
    execute 'edit ' . s:path
  endif
endfunction

nnoremap <silent> <leader>2 :call <SID>EditBuildFile()<CR>

function! MoveFile(newspec, bang) abort
  let s:old = expand('%')
  " could be improved:
  if (s:old == a:newspec)
    return 0
  endif

  if isdirectory(a:newspec)
    let s:new = a:newspec . '/' . fnamemodify(s:old, ':t')
  else
    let s:new = a:newspec
  endif

  let s:oldperms = getfperm(s:old)
  exe 'sav' . a:bang fnameescape(s:new)
  call setfperm(s:new, s:oldperms)
  call delete(s:old)
endfunction

command! -nargs=1 -complete=file -bang MoveFile call MoveFile('<args>', '<bang>')

nnoremap <silent> + :let @/ .= '\\|\<'.expand('<cword>').'\>'<cr>:call histadd('search', @/)<cr>n
