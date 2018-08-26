"" GENERAL SETTINGS
set nocompatible
filetype off
set wildmenu
set wildignore+=*.pyc
set autoindent
" Enable fuzzy finder
set path+=** 
" set swp directory
set directory^=$HOME/.vim/tmp// 
" System Copy
vnoremap <C-c> "*y :let @+=@*<CR>
" System Paste
"map <C-v> "+P
" Allow backspacing through autoindents
set backspace=indent,eol,start 
" UTF8 
"set encoding=UTF-8
"linux copy/paste
"set clipboard+=unnamed
"set paste
"set go+=a

""" COLOR/UI
" Match tabline background with unselected tabs
hi TabLineFill ctermfg=7
" Syntax Highlighting 
let python_highlight_all=1
syntax on
colorscheme solarized
" Line Numbers
set number
set numberwidth=2
" Highlight characters past 80
"highlight OverLength ctermbg=red ctermfg=white 
"match OverLength /\%81v.\+/

""" SPACES AND TABS
" Standard Tabs
set tabstop=2
"set softtabstop=2
set shiftwidth=2
set expandtab
" Flag White Space
au BufRead, BufNewFile *.py, *.pyw, *.c, *.h match BadWhitespace /\s\+$/
"" Filetype Specifics
" PEP8
au BufNewFile,BufRead *.py set tabstop=4
"au BufNewFile,BufRead *.py set softtabstop=4
au BufNewFile,BufRead *.py set shiftwidth=4
"au BufNewFile,BufRead *.py set textwidth=79
au BufNewFile,BufRead *.py set expandtab
au BufNewFile,BufRead *.py set autoindent
au BufNewFile,BufRead *.py set fileformat=unix
" JS/HTML 
au BufNewFile,BufRead *.js,*.html set tabstop=2
au BufNewFile,BufRead *.js,*.html set softtabstop=2
au BufNewFile,BufRead *.js,*.html set shiftwidth=2
au BufNewFile,BufRead *.js,*.html set expandtab
au BufNewFile,BufRead *.js let javaScript_fold=1 
" Haskell
au BufNewFile, BufRead *.hs set tabstop=4
au BufNewFile,BufRead *.hs set softtabstop=4
au BufNewFile,BufRead *.hs set shiftwidth=4
au BufNewFile,BufRead *.hs set textwidth=79
au BufNewFile,BufRead *.hs set expandtab
au BufNewFile,BufRead *.hs set autoindent
au BufNewFile,BufRead *.hs set fileformat=unix

" txt files
autocmd BufNewFile,BufRead *.txt set tw=72

""" FOLDING
"set foldmethod=syntax
"set foldlevelstart=99 
set nofoldenable


""" KEY MAPPINGS
" Spacebar as leader
nnoremap <SPACE> <Nop>
let mapleader = "\<Space>" 
" Leader-f fold 
nnoremap <Leader>f za
" Map ctrl-s to save
nmap <c-s> :w<CR>
imap <c-s> <Esc>:w<CR>a
" Map Leader P to replace word with buffer 0
map <Leader>p cw<C-r>0<ESC>
" Map enter to insert after cursor
nmap <CR> o<Esc>
" Map Leader Square Brackets for cycling window buffers
map <Leader>] :bnext<CR>
map <Leader>[ :bprevious<CR>
" select all text in buffer
map <Leader>a ggVG

" Right/Left Arrows
imap <c-l> <space>->
imap <c-h> <space><- 


" Map :W to :w and :Q to :q
if has("user_commands")
    command! -bang -nargs=? -complete=file E e<bang> <args>
    command! -bang -nargs=? -complete=file W w<bang> <args>
    command! -bang -nargs=? -complete=file Wq wq<bang> <args>
    command! -bang -nargs=? -complete=file WQ wq<bang> <args>
    command! -bang Wa wa<bang>
    command! -bang WA wa<bang>
    command! -bang Q q<bang>
    command! -bang QA qa<bang>
    command! -bang Qa qa<bang>
endif


""" TMUX/SYSTEM RELATED
" yank to clipboard
"if has("clipboard")
"  set clipboard=unnamed " copy to the system clipboard
"
"  if has("unnamedplus") " X11 support
"    set clipboard+=unnamedplus
"  endif
"endif

"" Send currently focused filename to tmux status bar
"autocmd BufEnter * let &titlestring = ' ' . expand("%:t")
"set title
"set t_ts=k
"let &titleold='bash'


""" PLUGIN RELATED
" YouCompleteMe
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_autoclose_preview_window_after_completion = 1
"let g:ycm_python_binary_path = '/usr/local/bin/python'
nnoremap <Leader>j :YcmCompleter GoTo <CR>

command! -register JsDoc call jsdoc#insert()

" Solarized
let g:solarized_termtrans = 1

" Enable vim-slime with tmux
"let g:slime_target = "tmux"
"let g:slime_default_config = {"socket_name": split($TMUX, ",")[1], "target_pane": ":.1"}
"let g:slime_python_ipython = 1
