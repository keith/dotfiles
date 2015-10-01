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

execute pathogen#infect()

filetype plugin indent on " Re-enable after setup
syntax enable " Enable vim syntax highlighting as is (enable != on)

let s:fzf = system("brew --prefix fzf")
if v:shell_error == 0
  execute 'set rtp+=' . s:fzf
endif

" Load MatchIt for % jumping
runtime macros/matchit.vim
" Load man page plugin for :Man command
runtime ftplugin/man.vim

" Use space as leader!
let mapleader="\<Space>"

" I - Disable the startup message
" a - Avoid pressing enter after saves
set shortmess=Ia

set shell=$SHELL               " Set the default shell
set termencoding=utf-8         " Set the default encodings just in case $LANG isn't set
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

" Write undo tree to a file to resume from next time the file is opened
if has("persistent_undo")
  set undolevels=2000            " The number of undo items to remember
  set undofile                   " Save undo history to files locally
  set undodir=$HOME/.vimundo     " Set the directory of the undofile
  if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
  endif
endif

set directory=$HOME/.tmp/vim
if !isdirectory(expand(&directory))
  call mkdir(expand(&directory), "p")
endif

" On quit reset title
let &titleold=getcwd()

set background=dark
silent! colorscheme parsec

set ttyfast                 " Set that we have a fast terminal
set t_Co=256                " Explicitly tell Vim that the terminal supports 256 colors
set lazyredraw              " Don't redraw vim in all situations
set synmaxcol=300           " The max number of columns to try and highlight
set noerrorbells            " Don't make noise
set autoread                " Watch for file changes and auto update
set showmatch               " Set show matching parenthesis
set matchtime=2             " The amount of time matches flash
set display=lastline        " Display super long wrapped lines
set number                  " Shows line numbers
set relativenumber
set cursorline              " Highlight the line the cursor is on
set nrformats-=octal        " Never use octal notation
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
set foldlevelstart=99       " Set the default level of open folds
set foldmethod=indent       " Decide where to fold based
set foldnestmax=5           " Set deepest fold to x levels
set exrc                    " Source local .vimrc files
set secure                  " Don't load autocmds from local .vimrc files
set colorcolumn=+1          " Show a line past the text width

" Default text width to 80
if &textwidth == 0
  set textwidth=80
endif

" Don't have a text width for readonly files
if &readonly
  set textwidth=0
endif

" Completion options
set complete=.,w,b,u,t,i,kspell
set completeopt=menu
set wildmenu                                           " Better completion in the CLI
set wildmode=longest:full,full                         " Completion settings

" Ignore these folders for completions
set wildignore+=.hg,.git,.svn                          " Version control
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg         " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.pyc " compiled object files
set wildignore+=tags,.tags

" Dictionary for custom words
set dictionary+=/usr/share/dict/words
set spellfile=$HOME/.vim/custom-words.utf-8.add

" Set mapping and key timeouts
set timeout
set timeoutlen=1000
set ttimeoutlen=100

" Setting to indent wrapped lines
if exists("+breakindent")
  set breakindent
  set breakindentopt=shift:2
endif

" Check for file specific vim settings in the last 3 lines of the file
set modeline
set modelines=2

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
set statusline+=%{tagbar#currenttag('%s\ <\ ','','')}
set statusline+=%y
set statusline+=\ \ %P
set statusline+=-%l
set statusline+=-%c
set statusline+=\ %#ErrorMsg#%{SyntasticStatuslineFlag()}%*

if has("clipboard")     " If the feature is available
  set clipboard=unnamed " copy to the system clipboard

  if has("unnamedplus")
    set clipboard+=unnamedplus
  endif
endif

" Fuck you, help key.
noremap <F1> <Nop>

" Paging keys
inoremap <PageDown> <Nop>
inoremap <PageUp> <Nop>

nnoremap Q :q<CR>

" Easier save mapping
nnoremap W :write<CR>

" Disable K
vnoremap K <Nop>

" Reselect visual blocks after movement
vnoremap < <gv
vnoremap > >gv

" Move as expected on wrapped lines
noremap j gj
noremap gj j
noremap <Down> gj
inoremap <Down> <C-o>gj
noremap k gk
noremap gk k
noremap <Up> gk
inoremap <Up> <C-o>gk

" Computers are dumb
" scroll gets reset every time the window is resized
nnoremap <C-u> 10<C-u>
nnoremap <C-d> 10<C-d>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

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
nnoremap L g_
vnoremap L g_
nmap <tab> %

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

" Close all the lists
nnoremap <leader>q :call CloseLists()<CR>
function! CloseLists()
  lclose
  cclose
  pclose
  silent! TagbarClose
endfunction

command! ClearWhitespace :call ClearWhitespace()
function! ClearWhitespace()
  let _s=@/
  let line = line(".")
  let column = col(".")
  silent! %s/\s\+$//e
  let @/=_s
  call cursor(line, column)
endfunction

function! ClearWhitespaceIfExpected()
  if &ft =~ 'markdown'
    return
  endif

  call ClearWhitespace()
endfunction

" Unfuck my screen
nnoremap U :syntax sync fromstart<CR>:redraw!<CR>

" Running as diff
if &diff
  set modifiable
  set noreadonly
  if tabpagenr('$') == 1
    nnoremap ZZ :wqall<cr>
  endif
endif

" Position resume
function! PositionRecall()
  if &ft =~ 'gitcommit\|gitrebase'
    return
  endif

  if line("'\"") > 0 && line("'\"") <= line("$")
    execute "normal g`\"zz"
  endif
endfunction

" Window sizes
augroup window_sizes
  autocmd!

  " Set the minimum window width for vertical splits
  autocmd VimEnter * silent! set winwidth=80
  autocmd VimEnter * silent! set winminwidth=20
augroup END

" Various filetype settings
augroup ft_settings
  autocmd!

  " Comment string settings
  if empty(&commentstring) | setlocal commentstring=#\ %s | endif
  autocmd FileType c,cpp,go,objc,php setlocal commentstring=//\ %s

  " Save files on some focus lost events, like switching splits
  autocmd BufLeave,FocusLost * silent! wall

  " Don't auto insert a comment when using O/o for a newline
  autocmd VimEnter,BufRead * set formatoptions-=o

  " Return to the same position you left the file in
  autocmd BufRead * call PositionRecall()

  " Clear whitespace on save
  autocmd BufWritePre * call ClearWhitespaceIfExpected()

  autocmd CursorHold <buffer> checktime
augroup END

augroup cursorline
  autocmd!
  autocmd WinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

" ObjC curly brace error fix
let c_no_curly_error = 1
