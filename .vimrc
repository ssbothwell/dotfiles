set nocompatible
filetype off

" line numbers
set number
set numberwidth=2

" UTF8
set encoding=utf-8

" Fuzzy Finder
set path+=**
set wildmenu

" Style Guides
au BufNewFile,BufRead *.py set tabstop=4
au BufNewFile,BufRead *.py set softtabstop=4
au BufNewFile,BufRead *.py set shiftwidth=4
au BufNewFile,BufRead *.py set textwidth=79
au BufNewFile,BufRead *.py set expandtab
au BufNewFile,BufRead *.py set fileformat=unix

au BufNewFile,BufRead *.js,*.html,*.css,*.json set tabstop=2
au BufNewFile,BufRead *.js,*.html,*.css,*.json set softtabstop=2
au BufNewFile,BufRead *.js,*.html,*.css,*.json set shiftwidth=2
au BufNewFile,BufRead *.js,*.html,*.css,*.json set expandtab

au BufNewFile,BufRead *.txt set textwidth=72

au BufNewFile,BufRead *.hs set tabstop=8
au BufNewFile,BufRead *.hs set softtabstop=8
au BufNewFile,BufRead *.hs set shiftwidth=2
au BufNewFile,BufRead *.hs set expandtab

" Enable Syntax Highlighting
syntax enable 
set background=dark

colorscheme solarized

" Flag White Space
au BufRead, BufNewFile *.py, *.pyw, *.c, *.h match BadWhitespace /\s\+$/

" autoindent
set autoindent

set runtimepath^=~/.vim/bundle/ctrlp.vim

