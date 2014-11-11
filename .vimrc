filetype off
" Basic Shit
syntax on
set number " line numbers
"Set tab to 4 spaces
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set mouse=a
set clipboard=unnamed
set background=dark

" Highlight over 80 characters
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v. \+/

" cmap w!! %!sudo tee > /dev/null %  " Lol, don't use this on afs...
set t_Co=256 "256 color
set encoding=utf-8 "UTF-8 character encoding
set tabstop=4  "4 space tabs
set shiftwidth=4  "4 space shift
set softtabstop=4  "Tab spaces in no hard tab mode
set expandtab  " Expand tabs into spaces
set autoindent  "autoindent on new lines
set showmatch  "Highlight matching braces
set ruler  "Show bottom ruler
set equalalways  "Split windows equal size
set formatoptions=croq  "Enable comment line auto formatting
set wildignore+=*.o,*.obj,*.class,*.swp,*.pyc "Ignore junk files
set title  "Set window title to file
set hlsearch  "Highlight on search
set ignorecase  "Search ignoring case
set smartcase  "Search using smartcase
set incsearch  "Start searching immediately
set scrolloff=5  "Never scroll off
set wildmode=longest,list  "Better unix-like tab completion
set cursorline  "Highlight current line
set clipboard=unnamed  "Copy and paste from system clipboard
set lazyredraw  "Don't redraw while running macros (faster)
set autochdir  "Change directory to currently open file
set nocompatible  "Kill vi-compatibility
set wrap  "Visually wrap lines
set linebreak  "Only wrap on 'good' characters for wrapping
set backspace=indent,eol,start  "Better backspacing
set linebreak  "Intelligently wrap long files
set ttyfast  "Speed up vim
set nostartofline "Vertical movement preserves horizontal position

" Strip whitespace from end of lines when writing file
autocmd BufWritePre * :%s/\s\+$//e

" Syntax highlighting and stuff
filetype plugin indent on
syntax on

" FileType Commands
autocmd FileType c0 setlocal shiftwidth=2
autocmd FileType c0 setlocal tabstop=2
autocmd FileType c0 setlocal softtabstop=2
autocmd FileType c setlocal shiftwidth=2
autocmd FileType c setlocal tabstop=2
autocmd FileType c setlocal softtabstop=2

" Get rid of warning on save/exit typo
command WQ wq
command Wq wq
command W w
command Q q

" Tab completion
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
:set dictionary="/usr/dict/words"

" Persistent undo
set undodir=~/.vim/undodir
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer reload

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

""""""""""""""""""""""""""""""
" => Visual mode related
""""""""""""""""""""""""""""""
" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>

" Disable paste mode when leaving insert mode
au InsertLeave * set nopaste

" Map Ctrl-C to ESC (So it doesn't do anything weird, just fully switches)
nnoremap <C-c> <Esc>bamboo

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set statusline=%t[%{strlen(&fenc)?&fenc:'none'},%{&ff}]%h%m%r%y%=%c,%l/%L\ %P
set laststatus=2
call pathogen#infect()
" Set for nerdtree toggle
map <C-n> :NERDTreeToggle<CR>

" Points vimwiki to dropbox
let g:vimwiki_list = [{"path":"~/Dropbox/wiki"}]

" At end because it works here, I guess.
colorscheme zenburn

" Toggle paste mode
:nmap \o :set paste!<CR>
