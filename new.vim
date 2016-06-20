"
" .vimrc
" Evan Bergeron
"

set nocompatible " Must be first - changes other commands

filetype off
filetype plugin on
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'gmarik/Vundle.vim'

" Plugins -----------------------------------------------------------

" Visuals
Plugin 'altercation/vim-colors-solarized'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'bling/vim-airline'
Plugin 'w0ng/vim-hybrid'

" Useful programming utilities
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'ctrlp-vim/ctrlp.vim'    " new
Plugin 'Raimondi/delimitMate'
Plugin 'tpope/vim-surround'     " new
Plugin 'tpope/vim-commentary'
Plugin 'vim-latex/vim-latex'
Plugin 'LaTeX-Box-Team/LaTeX-Box'

Plugin 'christoomey/vim-tmux-navigator'
Plugin 'junegunn/goyo.vim'

Plugin 'ManOfTeflon/exterminator'
Plugin 'Superbil/llvm.vim'

" To get:
" vim-latex
" vim-easymotion
" goyo
" elm.vim
"
call vundle#end()

" Basics ------------------------------------------------------------

set smartindent
set tabstop=4           " 4 space tabs
set shiftwidth=4
set mouse=a
set laststatus=2        " Status bar at bottom
set expandtab           " Expand tabs into spaces
set autoindent          " autoindent on new lines
set showmatch           " Highlight matching braces
set number              " line numbers
set relativenumber
set ruler               " Show bottom ruler
set equalalways         " Split windows equal size
set title               " Set window title to file
set hlsearch            " Highlight on search
set ignorecase          " Search ignoring case
set smartcase           " Search using smartcase
set incsearch           " Start searching immediately
set scrolloff=5         " Never scroll off
set wildmode=longest,list  "Better unix-like tab completion
set cursorline          " Highlight current line
set clipboard=unnamed   " Copy and paste from system clipboard
set lazyredraw          " Don't redraw while running macros (faster)
set autochdir           " Change directory to currently open file
set wrap                " Visually wrap lines
set linebreak           " Only wrap on 'good' characters for wrapping
set nolist
set linebreak           " Intelligently wrap long files
set ttyfast             " Speed up vim
set nostartofline       " Vertical movement preserves horizontal position
if exists('&breakindent')
  set breakindent       " Indent wrapped lines to same level
endif

set backspace=indent,eol,start  "Better backspacing
set wildignore+=*.o,*.obj,*.class,*.swp,*.pyc "Ignore junk files

" Visuals ------------------------------------------------------------

syntax on
set background=dark
set t_Co=256
set term=screen-256color

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

set tags=~/memsql/tags

" Leader-d now opens ctag definition in a split
nnoremap <silent><Leader>d <C-w><C-]>
nnoremap <silent><Leader>o <C-w><C-]><C-w>T

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

" FileType Commands -------------------------------------------------

" Syntax highlighting for llvm
augroup filetype
  au! BufRead,BufNewFile *.ll     set filetype=llvm
augroup END

" Syntax highlighting for c0 and compiler's language fragments
au BufReadPost *.c0 set syntax=c
au BufReadPost *.l1 set syntax=c
au BufReadPost *.l2 set syntax=c
au BufReadPost *.l3 set syntax=c
au BufReadPost *.l4 set syntax=c
au BufReadPost *.l5 set syntax=c
au BufReadPost *.l6 set syntax=c

" Syntax highlighting for one liners
au BufReadPost *.1l set syntax=python

" Comment string for vim-commentary plugin for SML
autocmd FileType sml set commentstring=\(*\ %s\ *\)
autocmd FileType ocaml set commentstring=\(*\ %s\ *\)
autocmd FileType tex set commentstring=\%\ %s
autocmd FileType vim set commentstring=\"\ %s
au BufRead,BufNewFile *.sig sml filetype=sml

" Syntax highlighting for sage
augroup filetypedetect
  au! BufRead,BufNewFile *.sage,*.spyx,*.pyx setfiletype python
augroup END



" Plugin Commands ---------------------------------------------------

" Set for nerdtree toggle
map <C-n> :NERDTreeToggle<CR>

" Points vimwiki to dropbox
let g:vimwiki_list = [{"path":"~/Dropbox/wiki"}, {'path': "~/Dropbox/WestmarchesFall2015",
            \ "path_html" : "~/Dropbox/WestmarchesFall2015/html"}]
noremap <Leader>wh :VimwikiAll2HTML<CR>

let g:hybrid_custom_term_colors = 1
let g:hybrid_reduced_contrast = 1
colorscheme hybrid

" Syntastic
let g:syntastic_python_python_exec="/usr/bin/python2"
let g:syntastic_sml_smlnj_args = "-m sources.cm"
let g:syntastic_sml_smlnj_fname = ""
au FileType sml let g:syntastic_always_populate_loc_list = 1
au FileType sml let g:syntastic_auto_loc_list = 1

" Mapping for zenroom mode
nnoremap <silent> <leader>g :Goyo<cr>

" airline
let g:airline_left_sep          = '>'
let g:airline_left_alt_sep      = ''
let g:airline_right_sep         = '<'
let g:airline_right_alt_sep     = ''

" vim-easy-align
vmap <Enter> <Plug>(EasyAlign)

autocmd BufWritePost *.tex :Latexmk

" For Merlin (Ocaml completion)
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
let g:syntastic_ocaml_checkers = ['merlin']
noremap <Leader>t :MerlinTypeOf<CR>

" Black magic from the almighty Tim Pope
" Allows you to say ys<text object>c<latex command>
" to wrap a text object in a latex command
let g:surround_{char2nr('c')} = "\\\1command\1{\r}"
