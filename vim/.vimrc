" Basic Changes "

set nocompatible " better vim
filetype off " required for Vundle
set rtp+=~/.vim/bundle/Vundle.vim " set runtime path to include Vundle
call vundle#begin() " initialize vundle


" Plugins "

Plugin 'VundleVim/Vundle.vim' " let Vundle manage Vundle, required
Plugin 'AutoClose' " autoclose pairs of items
Plugin 'lervag/vimtex' " modern vim plugin for editing LaTeX files
Plugin 'mattn/emmet-vim' " easier HTML/CSS editing
Plugin 'mxw/vim-jsx' "JSX syntax highlight and indenting
Plugin 'pangloss/vim-javascript' " improve JS indentation and syntax
Plugin 'scrooloose/syntastic' " syntax highlighing
Plugin 'reedes/vim-wordy' " uncover usage problems in writing
Plugin 'tpope/vim-commentary' " comment lines with shortcut
Plugin 'tpope/vim-fugitive' " Git wrapper
Plugin 'tpope/vim-repeat' " add Vim repeat convention to other
Plugin 'tpope/vim-speeddating' " use Ctrl-A/Ctrl-X to increment values
Plugin 'tpope/vim-surround' " surround objects easier
Plugin 'kalafut/vim-taskjuggler' " syntax for TaskJuggler
call vundle#end() " add all plugins before this line
filetype plugin indent on


" General Settings "

set ffs=unix,dos,mac " use Unix as standard file type"
set encoding=utf8 " set utf8 as standard encoding
set autoread " set to auto read when file is changed from outside Vim
let mapleader = " " " remap leader to space
set noswapfile " no swap file


" VIM User Interface (UI) "

colorscheme pablo
set number " display line numbers
set ruler " set ruler to show where I'm at
set relativenumber " toggle relative numbering
set listchars=tab:>-,trail:- " highlight tabs and trailing spaces
set list " enable highlighting indicated above
set cursorline " set cursor vertical line
let g:markdown_fenced_languages = ['html', 'python', 'javascript', 'bash=sh', 'r', 'scala=java', 'java', 'scheme']
2mat ErrorMsg '\%81v.' " toggle wrapping of text
syn match markdownIgnore "\$.*_.*\$" " ignore TeX math notation in Markdown file
set showcmd "show partial commands and visual mode selection size
set matchpairs+=<:> "enable %-matching for angle brackets <>

" VIM User Experience (UX) "

inoremap jk <esc>
set splitbelow " make window split below
set splitright " make window split to the right
set so=7 " set 7 lines up/down of cursor when moving vertically
set backspace=eol,start,indent " configure backspace
set shiftround " round indentation to nearest shiftwidth
set breakindent " keep indentation when wrapping lines
set nowrapscan " do not wrap around when searching
nnoremap ; :
nnoremap j gj
nnoremap k gk
set path+=** " search recursively through files within other folders
set wildmenu " show options above commands
" set mouse=a " Allow mouse to mouse cursor

" Justifies current block of text
" https://twitter.com/vsbuffalo/status/1331313958754521088
nmap <space><space> gwip


" Searching "

set incsearch " make search act like search in modern browsers
set ignorecase " ignore case when searching
set smartcase " ...but keep case if specified


" File Browsing "

let g:netrw_banner=0 " disable banner
let g:netrw_browse_split=4 " open in prior window
let g:netrw_altv=1 " open splits to the right
let g:netrw_liststyle=3 " tree view


" Leader Mapping Shortcuts "

nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel"
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel'
nnoremap <leader>ev :split $MYVIMRC<cr> " easy opening of .vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr> " reload vimrc from within vim
nnoremap <leader>g :SyntasticToggleMode<cr>
nnoremap <leader>w :set wrap!<cr> " toggle wrapping of text
nnoremap <leader>l :buffers<CR>
nnoremap <leader>n :bnext<CR>
nnoremap <leader>p :bprev<CR>
nnoremap <leader>t :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>
nnoremap <leader>T :set expandtab tabstop=8 shiftwidth=8 softtabstop=4<CR>
nnoremap <leader>M :set noexpandtab tabstop=8 softtabstop=4 shiftwidth=4<CR>
nnoremap <leader>m :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>
nnoremap <leader>n :bn<cr>
nnoremap <leader>p :bp<cr>
nnoremap <leader>d :bd<cr>


" Status Line Setup "

set laststatus=2
set statusline=
set statusline+=%7*\[%n]                                  "buffernr
set statusline+=%1*\ %<%F\                                "File+path
set statusline+=%2*\ %y\                                  "FileType
set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
set statusline+=%4*\ %{&ff}\                              "FileFormat
set statusline+=%5*\ %{&spelllang}\%{HighlightSearch()}\  "Spellang+HiLight on?
set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\             "Rownumber/total (%)
set statusline+=%9*\ col:%03c\                            "Column number
set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Mod? Read? Top/bot
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


" Colors and Fonts "

" enable syntax highlighting
if !exists("g:syntax_on")
    syntax enable
endif


" File Specific Settings "

" Spell check in Org-mode, Markdown, text, and LaTeX files
au BufNewFile,BufRead,BufNewFile *.org,*.md,*.tex,*.txt setlocal spell

" Set syntax highlighting for alternative files
au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown " Markdown syntax
au BufNewFile,BufFilePre,BufRead *.Rmd,*.rmd set filetype=markdown " Rmd syntax
au BufNewFile,BufFilePre,BufRead *.scala set filetype=java " Scala syntax

" General Python PEP8 settings "
au BufNewFile,BufFilePre,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" General JavaScript and JSX settings "
au BufNewFile,BufFilePre,BufRead *.js,*.jsx
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=2 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" Edit SQL in Vim "
au BufRead /tmp/psql.edit.* set syntax=sql

" Text, Tab, and Indent Related "

set autoindent " auto-indent new lines
set smartindent " set smart indent with new lines
set linebreak " always wrap long lines by words
set expandtab " change tab to spaces
set tw=79 " change text width to 80
set smarttab " be smarter with tabs
set tabstop=4
set shiftwidth=4
inoremap <S-Tab> <C-V><Tab>


" git commit experience "

autocmd FileType gitcommit setlocal spell " spell check in Git commit message
au FileType gitcommit set tw=72 " wrap git commit messages to 72 characters


" emmet customization "

let g:user_emmet_leader_key='<C-E>' " redefine trigger key


" LaTeX Suite settings "

let g:tex_flavor='latex' " changes default filetype to .tex instead of plaintex


" syntastic settings "

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
