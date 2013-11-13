"
" Keith Smiley's (http://keith.so) vimrc, do with what you will
"
"

set nocompatible " This must be first, because it changes other options

" Plugin setup ----------------------------- {{{
filetype off " Required for Vundle setup
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle bundles
" let Vundle manage Vundle
Bundle 'gmarik/vundle'

" Github repos user/repo
Bundle 'airblade/vim-gitgutter'
Bundle 'altercation/vim-colors-solarized'
Bundle 'b4winckler/vim-objc'
Bundle 'bling/vim-airline'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'ervandew/supertab'
Bundle 'evanmiller/nginx-vim-syntax'
Bundle 'godlygeek/tabular'
Bundle 'majutsushi/tagbar'
Bundle 'Raimondi/delimitMate'
Bundle 'rhysd/clever-f.vim'
Bundle 'scrooloose/syntastic'
Bundle 'sjl/clam.vim'
Bundle 'thoughtbot/vim-rspec'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-liquid'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-pathogen'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'vim-ruby/vim-ruby'
Bundle 'vim-scripts/cpp.vim--Skvirsky'
Bundle 'Yggdroot/indentLine'

" Vim script repos
Bundle 'a.vim'
Bundle 'IndexedSearch'

" Enable pathogen for plugin development
" Using {} at the end of the path is horrible
"   because it searches all folders need
"   pathogen support to specify wrapping folder
execute pathogen#infect('src/{}', $GOROOT . '/misc/{}')

" Setup go highlighting
" set rtp+=$GOROOT/misc/vim

filetype plugin indent on " Re-enable after Vundle setup
syntax enable " Enable vim syntax highlighting as is (enable != on)

" }}}

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
set undolevels=200             " The number of undo items to remember
set backspace=indent,eol,start " Backspace settings
set nostartofline              " Keep cursor in the same place after saves

set wildmenu                   " Better completion in the vim command line
set wildmode=longest,list,full " Completion settings
" Ignore these folders for completions
set wildignore+=.hg,.git,.svn  " Version control
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files

" Fold settings
set nofoldenable               " Have all folds open by default
set foldnestmax=10             " Set deepest fold to x levels
set foldmethod=indent          " Decide where to fold based off syntax
set foldcolumn=2               " The width of the gutter column showing folds by line
" Toggle folds with the space bar
nnoremap <Space> za

" Load MatchIt for % jumping
runtime! macros/matchit.vim

let &titleold=getcwd() " On quit reset title

let hour = strftime("%H") " Set the background light from 7am to 7pm
if 7 <= hour && hour < 19
  set background=light
else " Set to dark from 7pm to 7am
  set background=dark
endif
colorscheme solarized " Use the awesome solarized color scheme

set ttyfast           " Set that we have a fast terminal
set lazyredraw        " Don't redraw vim in all situations
set synmaxcol=300     " The max number of columns to try and highlight
set noerrorbells      " Don't make noise
set visualbell        " Don't show bells
set autoread          " watch for file changes and auto update
set showmatch         " set show matching parenthesis
set matchtime=1       " The amount of time matches flash
set display=lastline  " Display super long wrapped lines
set number            " Shows line numbers
set ruler             " Shows current cursor location
set nrformats-=octal  " Never use octal notation
set mouse=a           " enable using the mouse if terminal emulator
set hlsearch          " Highlight search terms
set incsearch         " Show searches as you type
set wrap              " Softwrap text
set linebreak         " Don't wrap in the middle of words
set ignorecase        " ignore case when searching
set smartcase         " ignore case if search is lowercase, otherwise case-sensitive
set title             " Change the terminal's title
set nobackup          " Don't keep backup files
set nowritebackup     " Don't create a backup when overwriting a file
set noswapfile        " Don't write swap files
set updatetime=4000   " Set the time before plugins assume you're not typing
set list              " Show hidden characters
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮ " Use these characters for typically hidden chars

" Quicker timeouts for tmux + vim + iTerm
set notimeout
set ttimeout
set ttimeoutlen=20

" Reselect visual blocks after movement
vnoremap < <gv
vnoremap > >gv

" Move as expected on wrapped lines
nnoremap j gj
vnoremap j gj
nnoremap <Down> gj
vnoremap <Down> gj
inoremap <Down> <C-o>gj

nnoremap k gk
vnoremap k gk
nnoremap <Up> gk
vnoremap <Up> gk
inoremap <Up> <C-o>gk

" Force root permission saves
cnoremap w!! !sudo tee % >/dev/null

" Open vimrc with leader->v
nnoremap <leader>v  :tabedit $MYVIMRC<cr>

if has("clipboard")     " If the feature is available
  set clipboard=unnamed " copy to the system clipboard
endif

" Tab mappings
nnoremap <leader>tt :tabnew<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove

" Split window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Change the way splits open by default
set splitbelow
set splitright

" Even out splits when vim is resized
autocmd VimResized * :wincmd =

" Objective-C matching bracket shortcuts
inoremap <C-n> <ESC>^i[<ESC>
nnoremap <C-n> ^i[<ESC>

" Remove the last search thus clearing the highlight
" This clears the search register denoted by @/
nnoremap <F4> :let @/ = ""<CR>

" http://vim.wikia.com/wiki/Toggle_auto-indenting_for_code_paste
nnoremap <F2> :set invpaste paste?<CR> " Toggle paste in normal mode
set pastetoggle=<F2> " Toggle paste in insert mode
set showmode         " Display the mode when it changes

" http://stackoverflow.com/a/12141458/902968
let g:ruby_path = system('echo $HOME/.rbenv/shims')

" Make sure ObjC header files are treated properly
autocmd BufReadPost,BufNewFile *.h,*.m setlocal filetype=objc
autocmd BufReadPost *Test.m,*Tests.m setlocal filetype=specta

autocmd BufReadPost,BufNewFile *.com setlocal filetype=nginx

" Set specific filetypes to fold by syntax
autocmd Syntax c,cpp,ruby,rspec,vim,xml,xhtml setlocal foldmethod=syntax
autocmd Syntax c,cpp,ruby,rspec,vim,xml,xhtml,perl normal zR

autocmd FileType markdown command! -buffer -bang Marked :!mark %
autocmd FileType markdown  setlocal textwidth=72
autocmd FileType make      setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType go        setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType php       setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType sh        setlocal tabstop=4 shiftwidth=4 expandtab
autocmd FileType python    setlocal tabstop=4 shiftwidth=4 expandtab nosmartindent " Fix issue where comments cannot be moved from the first column with >>
autocmd FileType objc      setlocal tabstop=4 shiftwidth=4 expandtab
autocmd FileType gitcommit setlocal spell
autocmd FileType vim       setlocal foldmethod=marker

" Settings for podspecs
autocmd BufNewFile,BufReadPost,BufWrite *.podspec setlocal filetype=ruby
autocmd BufNewFile,BufReadPost,BufWrite Podfile   setlocal filetype=ruby

" a.vim ObjC settings
autocmd FileType objc let g:alternateExtensions_h = "m"
autocmd FileType objc let g:alternateExtensions_m = "h"

" ObjC curly brace error fix
let c_no_curly_error = 1

" Cold Fusion correct comment type
autocmd FileType c       setlocal commentstring=//\ %s
autocmd FileType cf      setlocal commentstring=<!---\ %s\ --->
autocmd FileType cpp     setlocal commentstring=//\ %s
autocmd FileType crontab setlocal commentstring=#\ %s
autocmd FileType css     setlocal commentstring=//\ %s
autocmd FileType go      setlocal commentstring=//\ %s
autocmd FileType objc    setlocal commentstring=//\ %s
autocmd FileType php     setlocal commentstring=//\ %s

" Don't auto insert a comment when using O/o for a newline
autocmd FileType * setlocal formatoptions-=o

" Save files on some focus lost events, like switching splits
autocmd BufLeave,FocusLost * silent! wall

" Remap W to w http://stackoverflow.com/questions/3878692/aliasing-a-command-in-vim
cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))

" Powerline
set laststatus=2 " Always show the statusline
set t_Co=256     " Explicitly tell Vim that the terminal supports 256 colors

" CTRL-P
let g:ctrlp_show_hidden = 1
if exists('g:ctrlp_user_command')
  unlet g:ctrlp_user_command
endif
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

" Command for searching folders even if they
" aren't versioned with git
function! SearchCommand()
  let l:command = [
    \ 'if [ -d .git ] || git rev-parse --git-dir > /dev/null 2>&1;then',
    \   'git ls-files . --cached --exclude-standard --others',
    \ 'else',
    \   'find * -type f',
    \ 'fi'
    \ ]
  return join(l:command, "\n")
endfunction

" Run a given vim command on the results of fuzzy selecting from a given shell
" command. See usage below.
function! SelectaCommand(choice_command, selecta_args, vim_command)
  try
    silent let selection = system(a:choice_command . " | selecta " . a:selecta_args)
  catch /Vim:Interrupt/
    " Swallow the ^C so that the redraw below happens; otherwise there will be
    " leftovers from selecta on the screen
    redraw!
    return
  endtry
  redraw!
  exec a:vim_command . " " . selection
endfunction

" Find all files in all non-dot directories starting in the working directory.
" Fuzzy select one of those. Open the selected file with :e.
nnoremap <C-p> :call SelectaCommand(SearchCommand(), "", ":e")<cr>

" Airline
let g:airline_theme='solarized'
let g:airline_left_sep=''
let g:airline_right_sep=''

" TagBar
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

" Syntastic
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]
let g:syntastic_always_populate_loc_list=1
let g:syntastic_python_flake8_args="--ignore=E501"

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

" javascript libraries syntax
let g:used_javascript_libs = 'angularjs'

" Supertab
let g:SuperTabNoCompleteAfter = ['^', '\s', '#', '/']

" vim-rspec
let g:rspec_command = "Dispatch rspec {spec}"
nnoremap <Leader>f :call RunCurrentSpecFile()<CR>
nnoremap <Leader>s :call RunNearestSpec()<CR>
" nnoremap <Leader>l :call RunLastSpec()<CR>
" nnoremap <Leader>a :call RunAllSpecs()<CR>

" delimitMate don't match " in vim script
autocmd FileType vim let delimitMate_quotes = "' `"

" Local vimrc settings
if filereadable('.vimrc.local')
  source .vimrc.local
end

