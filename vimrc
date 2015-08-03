" set plugin manager
execute pathogen#infect()
filetype plugin indent on

"
" General Settings
"

" set to auto read when a file is changed from the outside
set autoread

" display line numbers
set number

" autoreload vimrc
augroup reload_vimrc " {
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

" never make a swap file
set noswapfile

"
" VIM User Interface
"

" set ruler
set ruler

" configure backspace 
set backspace=eol,start,indent

" highlight search results
" set hlsearch

" make search act like search in modern browsers
set incsearch

" toggle relative numbering
set rnu
function! ToggleNumbersOn()
    set nu!
    set rnu
endfunction
function! ToggleRelativeOn()
    set rnu!
    set nu
endfunction
autocmd FocusLost * call ToggleRelativeOn()
autocmd FocusGained * call ToggleRelativeOn()
autocmd InsertEnter * call ToggleRelativeOn()
autocmd InsertLeave * call ToggleRelativeOn()

"
" Colors and Fonts
"

" enable syntax highlighting
syntax enable
syntax on

" 
" Text, Tab, and Indent Related
"

" auto-indent new lines
set autoindent

" always wrap long lines
set wrap

" change tab to spaces
set expandtab

" change text width to 80
set tw=80

" be smarter with tabs
set smarttab

" 1 tab == 4 spaces
set tabstop=4
set shiftwidth=4

set ai " auto indent
set si " smart indent

" 
" emmet customization
"

" redefine trigger key
let g:user_emmet_leader_key='<C-E>'

"
" LaTeX Suite settings
"

" changes the default filetype back to .tex instead of plaintex
let g:tex_flavor='latex'
