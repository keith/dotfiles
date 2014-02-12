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

" Github repos user/repo
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
Bundle 'jnwhiteh/vim-golang'
Bundle 'justinmk/vim-gtfo'
Bundle 'Keithbsmiley/investigate.vim'
Bundle 'Keithbsmiley/rspec.vim'
Bundle 'Keithbsmiley/specta.vim'
Bundle 'Keithbsmiley/tmux.vim'
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
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-liquid'
Bundle 'tpope/vim-markdown'
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

try
  colorscheme solarized " Use the awesome solarized color scheme
catch /^Vim\%((\a\+)\)\=:E185/
endtry

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
set autoread         " watch for file changes and auto update
set showmatch        " set show matching parenthesis
set matchtime=2      " The amount of time matches flash
set display=lastline " Display super long wrapped lines
set number           " Shows line numbers
set ruler            " Shows current cursor location
set cursorline       " Highlight the line the cursor is on
set nrformats-=octal " Never use octal notation
set nojoinspaces     " Don't add 2 spaces when using J
set mouse=a          " enable using the mouse if terminal emulator
set hlsearch         " Highlight search terms
set incsearch        " Show searches as you type
set wrap             " Softwrap text
set linebreak        " Don't wrap in the middle of words
set ignorecase       " ignore case when searching
set smartcase        " ignore case if search is lowercase, otherwise case-sensitive
set title            " Change the terminal's title
set nobackup         " Don't keep backup files
set nowritebackup    " Don't create a backup when overwriting a file
set showmode         " Display the paste setting when it changes
set noswapfile       " Don't write swap files
set updatetime=4000  " Set the time before plugins assume you're not typing
set scrolloff=5      " Number of lines the cursor is to the edge before scrolling
set scroll=10        " Number of lines for CTRL-u/d jumps
set gdefault         " Adds g at the end of substitutions by default
set nolist           " Show/Hide hidden characters
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮ " Use these characters for typically hidden chars
silent! set colorcolumn=80   " Highlight column x

" Completion options
set complete=.,w,b,u,t,i
set completeopt=menu,preview
set wildmenu                                     " Better completion in the vim command line
set wildmode=longest,list,full                   " Completion settings
" Ignore these folders for completions
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files

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
nnoremap <leader>ev :tabedit $MYVIMRC<CR>

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
set winwidth=80         " Set the minimum window width for splits
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

" CSS ------ {{{
augroup ft_css_sass
  autocmd!
  autocmd FileType css setlocal commentstring=/*\ %s\ */
  autocmd FileType css,scss.css setlocal foldmethod=marker foldmarker={,}
  autocmd FileType css,scss.css autocmd BufReadPost *
        \ inoremap <buffer> {<CR> {<CR><CR>}<ESC>ki<tab>
augroup END
" }}}

" Nginx ------ {{{
augroup ft_nginx
  autocmd!
  autocmd BufRead,BufNewFile /*/nginx/conf/*            setlocal filetype=nginx
  autocmd BufRead,BufNewFile /*/nginx/sites-available/* setlocal filetype=nginx
  autocmd FileType nginx setlocal foldmethod=marker foldmarker={,}
  autocmd FileType nginx setlocal commentstring=#\ %s
augroup END
" }}}

" Git ------ {{{
augroup ft_git
  autocmd!
  autocmd BufReadPost *gitconfig     setlocal filetype=gitconfig
  autocmd FileType    gitcommit      setlocal spell
augroup END
" }}}

" Vim Help Files ------ {{{
augroup ft_help
  autocmd!
  autocmd BufRead,BufNewFile *.vim/doc/*.txt setlocal filetype=help
  autocmd BufRead,BufNewFile vim-*/doc/*.txt setlocal filetype=help
  autocmd FileType help setlocal iskeyword+=-
  autocmd FileType help autocmd FileWritePost,BufWritePost <buffer> call pathogen#helptags()
  autocmd FileType help if !&readonly | setlocal spell autoindent formatoptions+=2n | endif
augroup END
" }}}

" Markdown files ------ {{{
function! Marked()
  silent !mark %
  redraw!
endfunction

augroup ft_markdown
  autocmd!
  if has("mac")
    autocmd FileType markdown  command! -buffer -bang Marked :call Marked()
  endif
  autocmd FileType markdown setlocal textwidth=72 spell
augroup END
" }}}

" Liquid files ------ {{{
augroup ft_liquid
  autocmd!
  autocmd FileType liquid setlocal textwidth=72 spell
augroup END
" }}}

" Mail files ------ {{{
augroup ft_mail
  autocmd BufRead,BufNewFile *mutt-* setfiletype mail
  autocmd FileType mail setlocal textwidth=78
  autocmd FileType mail setlocal spell
  autocmd FileType mail let g:airline#extensions#whitespace#enabled = 0
augroup END
" }}}

" Clojure files ------ {{{
augroup ft_clojure
  autocmd!
  autocmd FileType clojure setlocal foldmethod=marker foldmarker=(,)
  autocmd FileType clojure RainbowParenthesesActivate
  autocmd syntax clojure RainbowParenthesesLoadRound
  autocmd syntax clojure RainbowParenthesesLoadSquare
augroup END
" }}}

" Ruby files ------ {{{
augroup ft_ruby
  autocmd!
  autocmd FileType ruby set iskeyword+=?
  autocmd BufRead,BufNewFile *_spec.rb set syntax=rspec
  autocmd Syntax rspec let b:dispatch="rspec\ %"
augroup END
" }}}

" Haskell files ------ {{{
augroup ft_haskell
  autocmd!
  autocmd FileType haskell setlocal makeprg=runhaskell\ %
augroup END
" }}}

" Podspecs ------ {{{
augroup ft_podspec
  autocmd!
  autocmd BufNewFile,BufRead,BufWrite *.podspec setlocal filetype=podspec
  autocmd BufNewFile,BufRead Podfile setlocal filetype=podfile
  autocmd FileType podspec,podfile set syntax=ruby
  autocmd FileType podspec set makeprg=pod\ spec\ lint\ %
  autocmd FileType podfile set makeprg=pod\ install\ >/dev/null
augroup END
" }}}

" ObjC stuff ------ {{{
augroup ft_objc
  autocmd!
  autocmd BufReadPost,BufNewFile *.h,*.m,*.pch setlocal filetype=objc
  autocmd FileType objc setlocal tabstop=4 shiftwidth=4 expandtab

  " TODO: Fix this and specta.vim to use syntax for specta
  autocmd BufReadPost *Test.m,*Tests.m setlocal filetype=specta

  " Don't force lines starting with # to column 0 in pch files
  autocmd BufReadPost *.pch setlocal syntax=pch
  autocmd Syntax pch setlocal indentkeys-=0#
augroup END
" }}}

" Various filetype settings ------ {{{
augroup ft_settings
  autocmd!
  autocmd FileType json          setlocal foldmethod=syntax
  autocmd FileType make,go,php   setlocal tabstop=4 shiftwidth=4 noexpandtab
  autocmd FileType vim           setlocal foldmethod=marker
  autocmd FileType dcl           setlocal filetype=apache

  " Fix issue where comments cannot be moved from the first column with >>
  autocmd FileType python        setlocal tabstop=4 shiftwidth=4 expandtab nosmartindent
  autocmd FileType sh            setlocal tabstop=4 shiftwidth=4 expandtab

  " Comment string settings
  if empty(&commentstring)
    setlocal commentstring=#\ %s
  endif
  autocmd FileType cf setlocal commentstring=<!---\ %s\ --->
  autocmd FileType conkyrc,crontab setlocal commentstring=#\ %s
  autocmd FileType c,cpp,go,objc,php setlocal commentstring=//\ %s

  " Save files on some focus lost events, like switching splits
  autocmd BufLeave,FocusLost * silent! wall

  " Don't auto insert a comment when using O/o for a newline
  autocmd BufRead,BufReadPost,Syntax,VimEnter * setlocal formatoptions-=o

  " Return to the same position you left the file in
  autocmd BufReadPost * call PositionRecall()
augroup END
" }}}

" Position resume ------ {{{
function! PositionRecall()
  if &ft =~ 'gitcommit'
    return
  endif

  if line("'\"") > 0 && line("'\"") <= line("$") |
    execute "normal g`\"zz" |
  endif
endfunction
" }}}

function! EscapeFilePath(path)
  return substitute(a:path, ' ', '\\ ', 'g')
endfunction

" Custom alternate header/implementation files functions ------ {{{
function! Alternate(cmd)
  if &modified
    echoerr "Save buffer before alternating"
    return 0
  endif

  let l:filename = expand("%:r") . "."
  let l:extension = expand("%:e")
  let l:alternates = ["h"]
  if l:extension == "h"
    let l:alternates = ["m", "c", "cpp", "mm"]
  endif
  let l:alternate = AlternateFor(l:filename, l:alternates)
  if l:alternate == ""
    echomsg "No alternate file for " . expand("%")
  else
    execute ":silent " . a:cmd . " " . EscapeFilePath(l:alternate)
  endif
endfunction

function! AlternateFor(filename, extensions)
  let l:alternate = ""
  for l:extension in a:extensions
    let l:possible = a:filename . l:extension
    if filereadable(l:possible)
      return l:possible
    endif
  endfor

  return l:alternate
endfunction

augroup alternate_headers
  autocmd!
  autocmd FileType objc,c,cpp command! -buffer A :call Alternate("edit")
  autocmd FileType objc,c,cpp command! -buffer AS :call Alternate("split")
  autocmd FileType objc,c,cpp command! -buffer AV :call Alternate("vsplit")
  autocmd FileType objc,c,cpp command! -buffer AT :call Alternate("tabnew")
augroup END
" }}}

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

" CTRL-P
let g:ctrlp_show_hidden = 1
if exists('g:ctrlp_user_command')
  unlet g:ctrlp_user_command
endif
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

" Selecta Git commands configuration ------ {{{
" Concatenate the directory into the ls-files command
function! GitListCommand(directory)
  return "git ls-files " . a:directory . " --cached --exclude-standard --others"
endfunction

" Command for searching folders even if they
" aren't tracked with git
function! SearchCommand()
  let l:command = ""
  if isdirectory(".git")
    let l:command = GitListCommand(".")
  endif

  if strlen(l:command) < 1
    let l:output = system("git rev-parse --show-toplevel")
    let l:output = substitute(l:output, "fatal: Not a git repository (or any of the parent directories): .git", "", "")
    let l:output = substitute(l:output, "\\n", "", "")
    if strlen(l:output) > 0
      let l:command = GitListCommand(l:output)
    else
      let l:command = "find * -type f"
    endif
  endif

  return l:command
endfunction

" Run a given vim command on the results of fuzzy selecting from a given shell
" command. See usage below.
function! SelectaCommand(choice_command, selecta_args, vim_command)
  try
    silent let selection = system(a:choice_command . " | selecta " . a:selecta_args)
  catch /Vim:Interrupt/
    " Catch the ^C so that the redraw below happens; otherwise there will be
    " leftovers from selecta on the screen
    redraw!
    return
  endtry
  redraw!
  exec a:vim_command . " " . EscapeFilePath(selection)
endfunction

" Find all files in all non-dot directories starting in the working directory.
" Fuzzy select one of those. Open the selected file with :e.
nnoremap <C-p> :call SelectaCommand(SearchCommand(), "", ":e")<cr>
nnoremap <C-t> :call SelectaCommand(SearchCommand(), "", ":tabnew")<cr>
nnoremap <leader>p :call SelectaCommand(SearchCommand(), "", ":vsplit")<cr>
nnoremap <leader>t :call SelectaCommand(SearchCommand(), "", ":split")<cr>
" }}}

" Airline
let g:airline_theme = 'solarized'
let g:airline_left_sep = ''
let g:airline_right_sep = ''

" TagBar ------ {{{
nnoremap <silent> <Leader>b :TagbarToggle<CR>
let g:tagbar_type_go = {
  \ 'ctagstype' : 'go',
  \ 'kinds'     : [
    \ 'p:package',
    \ 'i:imports:1',
    \ 'c:constants',
    \ 'v:variables',
    \ 't:types',
    \ 'n:interfaces',
    \ 'w:fields',
    \ 'e:embedded',
    \ 'm:methods',
    \ 'r:constructor',
    \ 'f:functions'
  \ ],
  \ 'sro' : '.',
  \ 'kind2scope' : {
    \ 't' : 'ctype',
    \ 'n' : 'ntype'
  \ },
  \ 'scope2kind' : {
    \ 'ctype' : 't',
    \ 'ntype' : 'n'
  \ },
  \ 'ctagsbin'  : 'gotags',
  \ 'ctagsargs' : '-sort -silent'
\ }
" }}}

" Syntastic ------ {{{
let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1

let g:syntastic_html_tidy_ignore_errors = [" proprietary attribute \"ng-"]
let g:syntastic_python_flake8_args = "--ignore = E501"
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_haskell_checkers = ['ghc-mod', 'hdevtools']
let g:hdevtools_options = '-g -Wall'

let s:compiler_options = '-std=gnu99 -fobjc-arc -fmodules'
let g:syntastic_objc_compiler = 'clang'
let g:syntastic_objc_gcc_quiet_messages = { "regex": 'file not found' }
let g:syntastic_objc_check_header = 1
let g:syntastic_objc_compiler_options = s:compiler_options
let s:module_cache = expand('$HOME') . '/Library/Developer/Xcode/DerivedData/ModuleCache'
if isdirectory(s:module_cache)
  let g:syntastic_objc_compiler_options .= ' -fmodules-cache-path=' . s:module_cache
endif
let s:pch_path = '*/*.pch'
if !empty(glob(s:pch_path))
  let b:syntastic_objc_cflags = '-include ' . s:pch_path
endif

" Allow toggling of syntastic errors list
" http://stackoverflow.com/questions/17512794/toggle-error-location-panel-in-syntastic
function! ToggleErrors()
  " Check the total number of open windows
  let old_last_winnr = winnr('$')
  " Attempt to close the location list
  lclose
  " If there are still the same number of windows
  " Open the errors list
  if old_last_winnr == winnr('$')
    Errors
  endif
endfunction
nnoremap <leader>e :call ToggleErrors()<cr>
" }}}

" clang_complete
let g:clang_library_path = '/Library/Developer/CommandLineTools/usr/lib'
let g:clang_user_options = s:compiler_options
let g:clang_snippets = 1
let g:clang_close_preview = 1
let g:clang_snippets = 1
let g:clang_complete_macros = 1
let g:clang_complete_patterns = 1
let g:clang_auto_select = 2
let g:clang_jumpto_back_key = "<C-5>"
let g:clang_complete_auto = 0
let g:clang_hl_errors = 0
let g:clang_complete_copen = 0
let g:clang_periodic_quickfix = 0

" Clever-f
let g:clever_f_across_no_line = 1

" Dispatch.vim
nnoremap <leader>d :Dispatch<CR>

" investigate.vim
nnoremap <silent> K :call investigate#Investigate()<cr>
let g:investigate_use_dash = 1
let g:investigate_use_url_for_haskell = 1

" vim-rooter
let g:rooter_manual_only = 1

" Local vimrc settings
if filereadable('.vimrc.local')
  source .vimrc.local
end
