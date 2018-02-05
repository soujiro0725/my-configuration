syntax enable
set number
set ruler
"
set list
"" 不可視文字に記号使ったり色つけたり...
set listchars=tab:»-,trail:¬,eol:↲,extends:»,precedes:«,nbsp:%
hi NonText ctermfg=66 guifg=#3a3a3a
hi SpecialKey ctermfg=66 guifg=#5f8787

set incsearch
set hlsearch
set nowrap
set showmatch
set whichwrap=h,l
set nowrapscan
set ignorecase
set smartcase
set hidden
set history=2000
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
set helplang=en

" for running python code in vim
nnoremap <buffer> <C-r> :exec '!python' shellescape(@%, 1)<cr>

