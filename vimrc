"
" Keith Smiley's (http://keith.so) vimrc, do with what you will
"
"

set nocompatible " This must be first, because it changes other options

" Source files before plugins
source ~/.vim/before/*.vim

" Plugin setup ------ {{{
filetype off " Required for Vundle setup

if empty(glob("~/.vim/autoload/pathogen.vim"))
  execute "!curl -fLo ~/.vim/autoload/pathogen.vim https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim"
endif

execute pathogen#infect()

filetype plugin indent on " Re-enable after Vundle setup
syntax enable " Enable vim syntax highlighting as is (enable != on)
" }}}

" Load MatchIt for % jumping
runtime macros/matchit.vim
" Load man page plugin for :Man command
runtime ftplugin/man.vim

" Remap the leader from \ to ,
let mapleader=","

" http://stackoverflow.com/questions/10507344/get-vim-to-modify-the-file-instead-of-moving-the-new-version-on-it
" http://www.jamison.org/2009/10/03/how-to-fix-the-crontab-no-changes-made-to-crontab-error-using-vim-in-linux/
set backupcopy=yes             " Allow vim to write crontab files

" I - Disable the startup message
" a - Avoid pressing enter after saves
set shortmess=Ia

set shell=$SHELL               " Set the default shell
set termencoding=utf-8         " Set the default encodings just in case $LANG isn't set
set encoding=utf-8             " Set the default encodings just in case $LANG isn't set
set autoindent                 " Indent the next line matching the previous line
set smartindent                " Smart auto-indent when creating a new line
set cursorline                 " highlight current line
set tabstop=2                  " Number of spaces each tab counts for
set shiftwidth=2               " The space << and >> moves the lines
set softtabstop=2              " Number of spaces for some tab operations
set shiftround                 " Round << and >> to multiples of shiftwidth
set expandtab                  " Insert spaces instead of actually tabs
set smarttab                   " Delete entire shiftwidth of tabs when they're inserted
set history=1000               " The number of history items to remember
set backspace=indent,eol,start " Backspace settings
set nostartofline              " Keep cursor in the same place after saves
set showcmd                    " Show command information on the right side of the command line

" Write undo tree to a file to resume from next time the file is opened
if has("persistent_undo")
  set undolevels=2000            " The number of undo items to remember
  set undofile                   " Save undo history to files locally
  set undodir=$HOME/.vimundo     " Set the directory of the undofile
  if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
  endif
endif

" Fold settings ------ {{{
set foldnestmax=5           " Set deepest fold to x levels
set foldmethod=indent       " Decide where to fold based
set foldlevelstart=99       " Set the default level of open folds

" Toggle folds with the space bar
nnoremap <Space> za
" }}}

" On quit reset title
let &titleold=getcwd()

set background=dark
silent! colorscheme parsec

set ttyfast          " Set that we have a fast terminal
set laststatus=2     " Always show the statusline
set t_Co=256         " Explicitly tell Vim that the terminal supports 256 colors
set lazyredraw       " Don't redraw vim in all situations
set synmaxcol=300    " The max number of columns to try and highlight
set noerrorbells     " Don't make noise
set visualbell       " Don't show bells
set autoread         " Watch for file changes and auto update
set showmatch        " Set show matching parenthesis
set matchtime=2      " The amount of time matches flash
set display=lastline " Display super long wrapped lines
set number           " Shows line numbers
set relativenumber
set ruler            " Shows current cursor location
set cursorline       " Highlight the line the cursor is on
set nrformats-=octal " Never use octal notation
set nojoinspaces     " Don't add 2 spaces when using J
set mouse=a          " Enable using the mouse if terminal emulator
set mousehide        " Hide the mouse on typing
set hlsearch         " Highlight search terms
set incsearch        " Show searches as you type
set wrap             " Softwrap text
set linebreak        " Don't wrap in the middle of words
set ignorecase       " Ignore case when searching
set smartcase        " Ignore case if search is lowercase, otherwise case-sensitive
set title            " Change the terminal's title
set nobackup         " Don't keep backup files
set nowritebackup    " Don't create a backup when overwriting a file
set noswapfile       " Don't write swap files
set updatetime=2000  " Set the time before plugins assume you're not typing
set scrolloff=5      " Lines the cursor is to the edge before scrolling
set sidescrolloff=5  " Same as scrolloff but horizontal
set gdefault         " Adds g at the end of substitutions by default
set virtualedit=block   " Allow the cursor to move off the side in visual block

" Default text width to 80
if &textwidth == 0
  set textwidth=80
endif

set colorcolumn=+1
autocmd BufRead * if &readonly | silent! set colorcolumn= | endif

" Completion options
set complete=.,w,b,u,t,i
set completeopt=menu
set wildmenu                                     " Better completion in the CLI
set wildmode=longest:full,full                   " Completion settings
" Ignore these folders for completions
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.pyc " compiled object files
set wildignore+=tags,.tags

" Dictionary for custom words
set spellfile=$HOME/.vim/custom-words.utf-8.add

" Setup stuff for mksession
set sessionoptions+=resize,localoptions,help

" Set mapping and key timeouts
set timeout
set timeoutlen=1000
set ttimeoutlen=100

" Setting to indent wrapped lines
silent! set breakindent
silent! set breakindentopt=shift:2

" Check for file specific vim settings in the last 3 lines of the file
set modeline
set modelines=2

if has("clipboard")     " If the feature is available
  set clipboard=unnamed " copy to the system clipboard

  if has("unnamedplus")
    set clipboard+=unnamedplus
  endif
endif

" Fuck you, help key.
noremap  <F1> <Nop>
inoremap <F1> <Nop>

" Paging keys
inoremap <PageDown> <Nop>
inoremap <PageUp> <Nop>

nnoremap Q :q<CR>

" Easier save mapping
nnoremap W :update<CR>

" Go backwards words
nnoremap E ge

" Disable K
vnoremap K <Nop>

" Don't select the last line in visual mode when jumping by block
vnoremap } j}k
vnoremap { k{j
" Sort in visual mode and update
vnoremap s :sort ui \| update <CR>

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
command! -bang W w<bang>

" Edit vimrc with mapping
nnoremap <leader>ov :call VimConf()<CR>
function! VimConf()
  silent! tabedit $MYVIMRC
  silent! vsplit ~/.vim
  wincmd h
endfunction

" Sort entire file unique
nnoremap <leader>sf :call SortFile()<CR>
function! SortFile()
  normal! miggvG
  sort ui
  normal! v`i
endfunction

" Tab mappings ------ {{{
nnoremap <leader>tt :tabnew<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove
" }}}

" Split window navigation ------ {{{
if !exists('$TMUX')
  nnoremap <C-h> <C-w>h
  nnoremap <C-j> <C-w>j
  nnoremap <C-k> <C-w>k
  nnoremap <C-l> <C-w>l
endif

" Mappings for split resizing
nnoremap + :resize +5<CR>
nnoremap _ :resize -5<CR>
nnoremap ( :vertical resize -5<CR>
nnoremap ) :vertical resize +5<CR>

" Change the way splits open by default
set splitbelow
set splitright

" Even out splits when vim is resized
augroup vim_splits
  autocmd!
  autocmd VimResized * :wincmd =
augroup END
" }}}

" Better movement
nnoremap H ^
vnoremap H ^
nnoremap L g_
vnoremap L g_
nnoremap <tab> %

" Switch to the last file
nnoremap <leader><leader> <C-^>

" Move to last edit location and put it in the center of the screen
nnoremap <C-o> <C-o>zz

" Objective-C matching bracket shortcuts
" inoremap <leader>o <ESC>^i[<ESC>
" nnoremap <leader>o ^i[<ESC>

" Remove the last search thus clearing the highlight
" This clears the search register denoted by @/
nnoremap <leader>4 :let @/ = ""<CR>

" Don't automatically jump on search
nnoremap * :keepjumps normal! mi*`i<CR>
nnoremap # :keepjumps normal! mi#`i<CR>

" Close the quickfix and location lists simultaneously
nnoremap <leader>q :call CloseLists()<CR>
function! CloseLists()
  lclose
  cclose
  pclose
  silent! TagbarClose
endfunction

" Clean trailing whitespace
nnoremap <silent> <leader>w :call ClearWhitespace()<CR>
function! ClearWhitespace()
  normal mi
  silent! %s/\s\+$//
  let @/=""
  update
  normal `i
endfunction

" Unfuck my screen
nnoremap U :syntax sync fromstart<CR>:redraw!<CR>

" http://stackoverflow.com/a/12141458/902968
let s:location = expand('$HOME/.rbenv/shims')
if filereadable(s:location)
  let g:ruby_path = s:location
else
  let s:output = system('rvm current')
  if v:shell_error == 0
    let g:ruby_path = s:output
  endif
end

" Running as diff ------ {{{
if &diff
  set modifiable
  set noreadonly
  if tabpagenr('$') == 1
    nnoremap ZZ :wqall<cr>
  endif
endif
" }}}

" Position resume ------ {{{
function! PositionRecall()
  if &ft =~ 'gitcommit\|gitrebase'
    return
  endif

  if line("'\"") > 0 && line("'\"") <= line("$")
    execute "normal g`\"zz"
  endif
endfunction
" }}}

" Window sizes ------ {{{
augroup window_sizes
  autocmd!

  " Set the minimum window width for splits
  autocmd VimEnter * silent! set winwidth=80
  autocmd VimEnter * silent! set winminwidth=20
  " Setup the height of vertical splits
  " https://www.destroyallsoftware.com/file-navigation-in-vim.html
  " Order is key
  autocmd VimEnter * silent! set winheight=7
  autocmd VimEnter * silent! set winminheight=7
  autocmd VimEnter * silent! set winheight=999
augroup END
" }}}

" ObjC curly brace error fix
let c_no_curly_error = 1

" Local vimrc settings
if filereadable('.vimrc.local')
  source .vimrc.local
end
