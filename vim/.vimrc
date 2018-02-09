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

" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.vim/bundle/neobundle.vim/

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!

NeoBundle 'alpaca-tc/alpaca_powertabline'
NeoBundle 'Lokaltog/powerline', { 'rtp' : 'powerline/bindings/vim'}
NeoBundle 'Lokaltog/powerline-fontpatcher'
" scala用syntax highlight
NeoBundle 'derekwyatt/vim-scala'

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

