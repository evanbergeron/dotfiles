" Vim color file
" Maintainer:   Evan Bergeron <bergeronej@gmail.com>
" Last Change:
" URL:

" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

" Finish if we are in a term lacking 256 color support
if ! has("gui_running") && &t_Co <= 255
    finish
endif

" your pick:
set background=dark	" or light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="spectre"

hi Normal         ctermfg=none

" OR

" highlight clear Normal
" set background&
" highlight clear
" if &background == "light"
"   highlight Error ...
"   ...
" else
"   highlight Error ...
"   ...
" endif

" A good way to see what your colorscheme does is to follow this procedure:
" :w
" :so %
"
" Then to see what the current setting is use the highlight command.
" For example,
" 	:hi Cursor
" gives
"	Cursor         xxx guifg=bg guibg=fg

" Uncomment and complete the commands you want to change from the default.

"hi Cursor
"hi CursorIM
hi CursorLine     ctermbg=240 cterm=none guibg=240
hi CursorLineNr   ctermfg=3
"hi Directory
"hi DiffAdd
"hi DiffChange
"hi DiffDelete
"hi DiffText
"hi ErrorMsg
"hi VertSplit
"hi Folded
"hi FoldColumn
"hi IncSearch
hi LineNr         ctermfg=238
hi ModeMsg        ctermfg=7 ctermbg=8
"hi MoreMsg
"hi NonText
hi Question       ctermfg=12
hi Repeat         ctermfg=246
hi Search         ctermfg=173 ctermbg=247
hi SpecialKey     ctermfg=181
hi StatusLine     ctermfg=8 cterm=bold
hi StatusLineNC   ctermfg=8
hi Title          ctermfg=232
hi Visual         ctermfg=146
"hi VisualNOS
"hi WarningMsg
"hi WildMenu
"hi Menu
"hi Scrollbar
"hi Tooltip

" syntax highlighting groups
hi Boolean          ctermfg=11
hi Comment          ctermfg=8
hi Conditional      ctermfg=146 cterm=bold
hi Constant       ctermfg=146
"hi Identifier
hi Operator       ctermfg=215 cterm=none
hi Number         ctermfg=174
hi Statement      ctermfg=3
hi PreCondit      ctermfg=9   cterm=none
hi PreProc        ctermfg=216
"hi Type
hi String         ctermfg=146
hi Special        ctermfg=180
hi SpecialComment ctermfg=216
hi SpecialChar    ctermfg=144
"hi Underlined
"hi Ignore
"hi Error
"hi Todo

