"
" .vimrc
" Evan Bergeron
"

" Basics ------------------------------------------------------------

set nocompatible " Must be first - changes other commands
syntax enable
filetype off
filetype plugin indent on
"Set tab to 4 spaces
set smartindent
set tabstop=4  "4 space tabs
set shiftwidth=4
set mouse=a
set background=dark
let g:solarized_termtrans = 1
" colorscheme solarized

cmap w!! %!sudo tee > /dev/null %  " Lol, don't use this on afs...
set t_Co=256 "256 color
set encoding=utf-8 "UTF-8 character encoding
set laststatus=2 " Status bar at bottom
set shiftwidth=4  "4 space shift
set softtabstop=4  "Tab spaces in no hard tab mode
set expandtab  " Expand tabs into spaces
set autoindent  "autoindent on new lines
set showmatch  "Highlight matching braces
set number " line numbers
set relativenumber
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
set wrap  "Visually wrap lines
set linebreak  "Only wrap on 'good' characters for wrapping
set nolist
set backspace=indent,eol,start  "Better backspacing
set linebreak  "Intelligently wrap long files
set ttyfast  "Speed up vim
set nostartofline "Vertical movement preserves horizontal position
if exists('&breakindent')
  set breakindent " Indent wrapped lines to same level
endif

" Remappings --------------------------------------------------------

let mapleader = "\<Space>"

" Get rid of warning on save/exit typo
command WQ wq
command Wq wq
command W w
command Q q

" Make navigating long, wrapped lines behave like normal lines
noremap <silent> k gk
noremap <silent> j gj
noremap <silent> 0 g0
noremap <silent> $ g$
noremap <silent> ^ g^
noremap <silent> _ g_

" Map Ctrl-C to ESC (So it doesn't do anything weird, just fully switches)
nnoremap <C-c> <Esc>bamboo

" Disable paste mode when leaving insert mode
au InsertLeave * set nopaste

" Toggle paste mode
:nmap \o :set paste!<CR>

" SyntaxAttr shortcut
map -a :call SyntaxAttr#SyntaxAttr()<CR>

" Relative Line Numbers
function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

" Toggle relative line numbers
nnoremap <C-n> :call NumberToggle()<cr>

" The essential: tab completion!
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
:set dictionary="/usr/dict/words"

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>

nnoremap <silent> <F5> :!clear;python2 %<CR>
nnoremap <silent> <F6> :!clear;python3 %<CR>

" Undo / Swap/ Backup -----------------------------------------------

" Persistent undo
set undodir=~/.vim/undodir
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer reload

" Backup and no Swap File
set backup
set noswapfile
set backupdir=~/.vim/tmp/backup
set directory=~/.vim/tmp/swap

" Strip whitespace from end of lines when writing file
autocmd BufWritePre * :%s/\s\+$//e

if has('nvim')
    " Mimic my tmux.conf
    :noremap <C-f> <C-w> " This will horribly break tmux, intended to replace it
    :tnoremap <Esc> <C-\><C-n> " Escape out of insert mode in term
    :noremap <Leader>" :sp term://zsh<CR>
    :noremap <Leader>v :vsp term://zsh<CR>
    :noremap <C-f>" :sp term://zsh<CR>
    :noremap <C-f>v :vsp term://zsh<CR>

    :noremap <Leader>t :sp term://zsh<CR>
    :noremap <Leader>T :vsp term://zsh<CR>
    :tnoremap <Leader>h <C-\><C-n><C-w>h
    :tnoremap <Leader>j <C-\><C-n><C-w>j
    :tnoremap <Leader>k <C-\><C-n><C-w>k
    :tnoremap <Leader>l <C-\><C-n><C-w>l
    :nnoremap <Leader>h <C-w>h
    :nnoremap <Leader>j <C-w>j
    :nnoremap <Leader>k <C-w>k
    :nnoremap <Leader>l <C-w>l
endif

" FileType Commands -------------------------------------------------

" Indentation for C / C0
autocmd FileType c0 setlocal shiftwidth=2
autocmd FileType c0 setlocal tabstop=2
autocmd FileType c0 setlocal softtabstop=2
autocmd FileType c setlocal shiftwidth=2
autocmd FileType c setlocal tabstop=2
autocmd FileType c setlocal softtabstop=2
autocmd FileType sml setlocal shiftwidth=2
autocmd FileType sml setlocal tabstop=2
autocmd FileType sml setlocal softtabstop=2
autocmd FileType ocaml setlocal shiftwidth=2
autocmd FileType ocaml setlocal tabstop=2
autocmd FileType ocaml setlocal softtabstop=2

" Indent two spaces for latex
autocmd FileType tex setlocal shiftwidth=2
autocmd FileType tex setlocal tabstop=2
autocmd FileType tex setlocal softtabstop=2

" Syntax highlighting for c0 and compiler's language fragments
au BufReadPost *.c0 set syntax=c
au BufReadPost *.l1 set syntax=c
au BufReadPost *.l2 set syntax=c
au BufReadPost *.l3 set syntax=c
au BufReadPost *.l4 set syntax=c
au BufReadPost *.l5 set syntax=c
au BufReadPost *.l6 set syntax=c

" Comment string for vim-commentary plugin for SML
autocmd FileType sml set commentstring=\(*\ %s\ *\)
autocmd FileType ocaml set commentstring=\(*\ %s\ *\)
au BufRead,BufNewFile *.sig sml filetype=sml

" Syntax highlighting for sage
augroup filetypedetect
  au! BufRead,BufNewFile *.sage,*.spyx,*.pyx setfiletype python
augroup END

" Plugin Commands ---------------------------------------------------

call pathogen#infect()

" Set for nerdtree toggle
map <C-n> :NERDTreeToggle<CR>

" Points vimwiki to dropbox
let g:vimwiki_list = [{"path":"~/Dropbox/wiki"}, {'path': "~/Dropbox/WestmarchesFall2015",
            \ "path_html" : "~/Dropbox/WestmarchesFall2015/html"}]
noremap <Leader>wh :VimwikiAll2HTML<CR>
" nnoremap <silent> <leader>g :Goyo<cr>


" Syntastic checks for python 2, not 3
let g:syntastic_python_python_exec="/usr/bin/python2"


" Mapping for zenroom mode
nnoremap <silent> <leader>g :Goyo<cr>

" airline
let g:airline_theme             = 'solarized'
let g:airline_left_sep          = '>'
let g:airline_left_alt_sep      = ''
let g:airline_right_sep         = '<'
let g:airline_right_alt_sep     = ''

" vim-easy-align
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
colorscheme spectre

" For Merlin (Ocaml completion)
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
let g:syntastic_ocaml_checkers = ['merlin']
noremap <Leader>t :MerlinTypeOf<CR>
