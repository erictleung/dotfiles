set nocompatible " better vim
filetype off " required for Vundle

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" plugins
Plugin 'scrooloose/syntastic'
Plugin 'mattn/emmet-vim'
" Plugin 'tomtom/tcomment_vim'
" Plugin 'msanders/snipmate.vim'
Plugin 'jiangmiao/auto-pairs'
Bundle 'vim-scripts/Vim-R-plugin'

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

" set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" use Unix as the standard file type
set ffs=unix,dos,mac

""" General Settings

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

""" VIM User Interface and Experience

" easier split navigations with ctrl
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" more natural split opening
set splitbelow
set splitright

" set ruler
set ruler

" configure backspace 
set backspace=eol,start,indent

" make search act like search in modern browsers
set incsearch

" ignore case when searching
set ignorecase

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

" set 7 lines up/down of the cursor - when moving vertically using j/k
set so=7

" turn on spell check for markdown files
autocmd BufRead,BufNewFile *.md setlocal spell

" turn on spell check for git commit messages
autocmd FileType gitcommit setlocal spell

" add keys to account for laziness
command WQ wq
command Wq wq
command W w
command Q q
nnoremap ; :

" use syntax highlighting in .md as .markdown
au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown
au BufNewFile,BufFilePre,BufRead *.scala set filetype=java

" status line setup
set laststatus=2
set statusline=
set statusline+=%7*\[%n]                                  "buffernr
set statusline+=%1*\ %<%F\                                "File+path
set statusline+=%2*\ %y\                                  "FileType
set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
set statusline+=%4*\ %{&ff}\                              "FileFormat (dos/unix..)
set statusline+=%5*\ %{&spelllang}\%{HighlightSearch()}\  "Spellang+HiLight on?
set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\             "Rownumber/total (%)
set statusline+=%9*\ col:%03c\                            "Column number
set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Modify? Read? Top/bot

function! HighlightSearch()
    if &hls
        return 'H'
    else
        return ''
    endif
endfunction

hi User1 guifg=#ffdad8  guibg=#880c0e
hi User2 guifg=#000000  guibg=#F4905C
hi User3 guifg=#292b00  guibg=#f4f597
hi User4 guifg=#112605  guibg=#aefe7B
hi User5 guifg=#051d00  guibg=#7dcc7d
hi User7 guifg=#ffffff  guibg=#880c0e gui=bold
hi User8 guifg=#ffffff  guibg=#5b7fbb
hi User9 guifg=#ffffff  guibg=#810085
hi User0 guifg=#ffffff  guibg=#094afe

" highlight tabs and trailing spaces
set listchars=tab:>-,trail:-
set list

""" Colors and Fonts

" enable syntax highlighting
syntax enable
syntax on

""" Text, Tab, and Indent Related

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

" Shift+Tab for tabs when necessary
inoremap <S-Tab> <C-V><Tab>

" for html/scala files, 2 space tabs
autocmd Filetype html setlocal ts=2 sw=2 expandtab
autocmd Filetype scala setlocal ts=2 sw=2 expandtab

set ai " auto indent
set si " smart indent

""" emmet customization

" redefine trigger key
let g:user_emmet_leader_key='<C-E>'

""" LaTeX Suite settings

" changes the default filetype back to .tex instead of plaintex
let g:tex_flavor='latex'

""" syntastic settings

" syntastic defaults until I understand this plugin better
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" R script settings
let maplocalleader = ","
vmap <Space> <Plug>RDSendSelection
nmap <Space> <Plug>RDSendLine
let vimrplugin_applescript=0
let vimrplugin_vsplit=1
