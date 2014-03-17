"
" Keith Smiley's (http://keith.so) vimrc, do with what you will
"
"

set nocompatible " This must be first, because it changes other options

" Plugin setup ------ {{{
filetype off " Required for Vundle setup
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle bundles
" let Vundle manage Vundle
Bundle 'gmarik/vundle'

Bundle 'airblade/vim-rooter'
Bundle 'altercation/vim-colors-solarized'
Bundle 'b4winckler/vim-objc'
Bundle 'bling/vim-airline'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'DasIch/vim-mercurial'
Bundle 'elzr/vim-json'
Bundle 'evanmiller/nginx-vim-syntax'
Bundle 'godlygeek/tabular'
Bundle 'hynek/vim-python-pep8-indent'
Bundle 'jnwhiteh/vim-golang'
Bundle 'justinmk/vim-gtfo'
Bundle 'Keithbsmiley/gist.vim'
Bundle 'Keithbsmiley/investigate.vim'
Bundle 'Keithbsmiley/kiwi.vim'
Bundle 'Keithbsmiley/rspec.vim'
Bundle 'Keithbsmiley/specta.vim'
Bundle 'Keithbsmiley/tmux.vim'
Bundle 'klen/python-mode'
Bundle 'majutsushi/tagbar'
Bundle 'Raimondi/delimitMate'
Bundle 'rhysd/clever-f.vim'
Bundle 'Rip-Rip/clang_complete'
Bundle 'rking/ag.vim'
Bundle 'scrooloose/syntastic'
Bundle 'sjl/clam.vim'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-jdaddy'
Bundle 'tpope/vim-liquid'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-projectile'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-vinegar'
Bundle 'vim-ruby/vim-ruby'

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
set foldcolumn=2            " The width of the gutter column showing folds by line
set foldlevelstart=99       " Set the default level of open folds

" Toggle folds with the space bar
nnoremap <Space> za
" }}}

" On quit reset title
let &titleold=getcwd()

" Setup colorscheme ------ {{{
" Make sure the override file exists
let s:filename = glob("$HOME/.coloroverride")
if s:filename != ""
  " Set the background to the contents of the override file
  execute 'set background=' . readfile(s:filename)[0]
else
  let hour = strftime("%H") " Set the background light from 7am to 7pm
  if 7 <= hour && hour < 19
    set background=light
  else " Set to dark from 7pm to 7am
    set background=dark
  endif
endif
silent! colorscheme solarized

" A functional equivalent to solarized background swap function
function! s:ToggleBackground()
  if &background ==# "light"
    set background=dark
  else
    set background=light
  endif
endfunction
silent! command ToggleBG call s:ToggleBackground()

" Set the color of the selected item in the autocomplete menu
highlight PmenuSel ctermfg=DarkYellow
" }}}

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
set showmode         " Display the paste setting when it changes
set noswapfile       " Don't write swap files
set updatetime=4000  " Set the time before plugins assume you're not typing
set scrolloff=5      " Lines the cursor is to the edge before scrolling
set gdefault         " Adds g at the end of substitutions by default
set report=0         " Report any number of line changes
set nolist           " Show/Hide hidden characters
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮ " Typically hidden chars

" Default text width to 80
if &textwidth == 0
  set textwidth=80
endif
" Highlight the columns after the textwidth
silent! let &colorcolumn='+'.join(range(1,255), ',+')
autocmd BufRead * if &readonly | silent! set colorcolumn= | endif
if &background ==# "dark"
  highlight ColorColumn ctermbg=Black
  highlight StatusLine ctermbg=DarkGray ctermfg=Black
else
  highlight ColorColumn ctermbg=LightGray
  highlight StatusLine ctermbg=DarkGrey ctermfg=White
endif

" Status line setup (without airline)
" Left Side
set statusline=\ \ %F
set statusline+=\ %r
" Right Side
set statusline+=%=
set statusline+=%Y
set statusline+=\ \ \ \ \ \ %P
set statusline+=\ :%l:
set statusline+=\ %c

" Completion options
set complete=.,w,b,u,t,i
set completeopt=menu
set wildmenu                                     " Better completion in the CLI
set wildmode=longest,list,full                   " Completion settings
" Ignore these folders for completions
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.pyc " compiled object files

set spellfile=$HOME/.vim/custom-words.utf-8.add

" Set mapping and key timeouts
set timeout
set timeoutlen=1000
set ttimeoutlen=100

" Check for file specific vim settings in the last 3 lines of the file
set modeline
set modelines=3

if has("clipboard")     " If the feature is available
  set clipboard=unnamed " copy to the system clipboard
endif

" Fuck you, help key.
noremap  <F1> <nop>
inoremap <F1> <nop>

" Paging keys
inoremap <PageDown> <nop>
inoremap <PageUp> <nop>

" Disable ex mode
nnoremap Q <Nop>

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

command! Q q
command! W w
cnoreabbrev ` ~
cnoreabbrev `` `

" Edit vimrc with mapping
nnoremap <leader>ov :tabedit $MYVIMRC<CR>

" Sort entire file unique
nnoremap <leader>sf :call SortFile()<CR>
function! SortFile()
  normal! mzggvG
  :sort ui
  normal! v`z
endfunction

" Next and Last ------ {{{
" Motion for 'next/last object'.  'Last' here means 'previous', not 'final'.
" Unfortunately the 'p' motion was already taken for paragraphs.
"
" Next acts on the next object of the given type in the current line, last acts
" on the previous object of the given type in the current line.
"
" Currently only works for (, [, {, b, r, B, ', and ".
"
" Some examples (C marks cursor positions, V means visually selected):
"
" din'  -> delete in next single quotes                foo = bar('spam')
"                                                      C
"                                                      foo = bar('')
"                                                                C
"
" canb  -> change around next parens                   foo = bar('spam')
"                                                      C
"                                                      foo = bar
"                                                               C
"
" vil"  -> select inside last double quotes            print "hello ", name
"                                                                        C
"                                                      print "hello ", name
"                                                             VVVVVV

onoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
xnoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
onoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>
xnoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>

onoremap al :<c-u>call <SID>NextTextObject('a', 'F')<cr>
xnoremap al :<c-u>call <SID>NextTextObject('a', 'F')<cr>
onoremap il :<c-u>call <SID>NextTextObject('i', 'F')<cr>
xnoremap il :<c-u>call <SID>NextTextObject('i', 'F')<cr>

function! s:NextTextObject(motion, dir)
  let c = nr2char(getchar())

  if c ==# "b"
    let c = "("
  elseif c ==# "B"
    let c = "{"
  elseif c ==# "r"
    let c = "["
  endif

  execute "normal! " . a:dir . c . "v" . a:motion . c
endfunction
" }}}

" Tab mappings ------ {{{
nnoremap <leader>tt :tabnew<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove

" Setup the format for the tab line syntax
function! Tabline()
  let s = ''
  for i in range(tabpagenr('$'))
    let tab = i + 1
    let winnr = tabpagewinnr(tab)
    let buflist = tabpagebuflist(tab)
    let bufnr = buflist[winnr - 1]
    let bufname = bufname(bufnr)
    let bufmodified = getbufvar(bufnr, "&mod")

    let s .= '%' . tab . 'T'
    let s .= (tab == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#')
    let s .= ' [' . tab .':'
    let s .= len(buflist) . ']:'
    let s .= (bufname != '' ? fnamemodify(bufname, ':t') . ' ' : '[No Name] ')

    if bufmodified
      let s .= '[+] '
    endif
  endfor

  let s .= '%#TabLineFill#'
  return s
endfunction
set tabline=%!Tabline()
" }}}

" Split window navigation ------ {{{
if !exists('$TMUX')
  nnoremap <C-h> <C-w>h
  nnoremap <C-j> <C-w>j
  nnoremap <C-k> <C-w>k
  nnoremap <C-l> <C-w>l
endif

" Set the minimum window width for splits
set winwidth=80
set winminwidth=20
" Setup the height of vertical splits
" https://www.destroyallsoftware.com/file-navigation-in-vim.html
" Order is key
set winheight=7
silent! set winminheight=7
set winheight=999

" Mappings for split resizing
nnoremap + :resize +5<CR>
nnoremap - :resize -5<CR>
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
nnoremap L g_
nnoremap <tab> %

" Switch to the last file
nnoremap <leader><leader> <C-^>

" Move to last edit location and put it in the center of the screen
nnoremap <C-o> <C-o>zz

" Objective-C matching bracket shortcuts
" inoremap <leader>o <ESC>^i[<ESC>
nnoremap <leader>o ^i[<ESC>

" Remove the last search thus clearing the highlight
" This clears the search register denoted by @/
nnoremap <leader>4 :let @/ = ""<CR>

" Close the quickfix and location lists simultaneously
nnoremap <leader>q :call CloseLists()<CR>
function! CloseLists()
  lclose
  cclose
  pclose
endfunction

" Clean trailing whitespace
nnoremap <silent> <leader>w :call ClearWhitespace()<CR>
function! ClearWhitespace()
  normal mi
  try
    %s/\s\+$//
  catch /^Vim\%((\a\+)\)\=:E486/
  endtry
  let @/=""
  normal `i
endfunction

command HashConvert call HashConvert()
function! HashConvert()
  normal mi
  %s/:\(\w*\)\(\s*\)=> /\1:\2/
  let @/=""
  normal `i
endfunction

" Unfuck my screen
nnoremap U :syntax sync fromstart<CR>:redraw!<CR>

" http://stackoverflow.com/a/12141458/902968
let g:ruby_path = system('echo $HOME/.rbenv/shims')

" Find TODO and FIXME comments
silent! command Todo call TODOSearch()
function! TODOSearch()
  if executable("ag") && exists(":Ag") > 0
    Ag 'TODO|FIXME'
  else
    noautocmd vimgrep /TODO\|FIXME/j ** | cw | nnoremap <silent> <buffer> q :cclose<CR>
  endif
endfunction

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
  if &ft =~ 'gitcommit'
    return
  endif

  if line("'\"") > 0 && line("'\"") <= line("$")
    execute "normal g`\"zz"
  endif
endfunction
" }}}

function! EscapeFilePath(path)
  return substitute(a:path, ' ', '\\ ', 'g')
endfunction

" Tab function ------ {{{
" let g:invalid_tab_chars = ['^', '\s', '#', '/', '\\', '*']
" function! TabWrapper()
"   let l:col = col('.') - 1
"   let l:lastchar = getline('.')[l:col - 1]
"   if l:col > 0 && (getline('.')[l:col - 1] =~ '\k' && index(g:invalid_tab_chars, l:lastchar) < 0)
"     return "\<C-p>"
"   else
"     return "\<tab>"
"   endif
" endfunction
" inoremap <tab> <C-r>=TabWrapper()<CR>
" }}}

" ObjC curly brace error fix
let c_no_curly_error = 1

" Local vimrc settings
if filereadable('.vimrc.local')
  source .vimrc.local
end
