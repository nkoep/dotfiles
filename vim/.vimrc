" Initialize pathogen. Install it first via:
"   mkdir -p ~/.vim/autoload ~/.vim/bundle && \
"   curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
execute pathogen#infect()

" Use vim settings, rather then vi settings. This must be first, because it
" changes other options as a side effect.
set nocompatible
set noswapfile

" Add vundle to the runtime path. Install vundle itself via:
"   git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tomtom/tcomment_vim'
Plugin 'dart-lang/dart-vim-plugin'
Plugin 'Valloric/YouCompleteMe'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
call vundle#end()

" {{{ General settings }}}
set fileformat=unix
set fileformats=unix,dos
set nobackup
" Remove trailing whitespace on save
augroup trail_save
  autocmd!
  autocmd BufWritePre * :%s/\s\+$//e
augroup END
" Add keywords TODO and FIXME to the current highlight group
autocmd! WinEnter,VimEnter *
  :silent! call matchadd('Todo', 'TODO\|FIXME\|XXX', -1)

syntax on
set splitright " open new vertically split buffers on the right
filetype plugin indent on
set showmode
set nowrap
set backspace=indent,eol,start " allow backspacing across lines in insert mode
set scrolloff=4 " keep four lines off the edges of the screen when scrolling
set gdefault " Use /g as default substitution behaviour in regexS
set termencoding=utf-8
set encoding=utf-8
set lazyredraw " don't update the display while executing macros
set laststatus=2 " always show a status bar
set cmdheight=2 " make status bar span across two rows
set history=1000 " increase size of command and search history buffer
set undolevels=1000 " increase size of undo buffer
let tex_no_error = 1 " turn off error highlighting in tex as it highlights
                     " underscores in non-math contexts
let g:tex_fast = ""
let g:ycm_server_python_interpreter = '/usr/bin/python'
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1

set laststatus=2 " Show airline without having to split a window first
let g:airline_theme = 'bubblegum'

" {{{ Indentation and tab rules }}}
set tabstop=2 " ts
set shiftwidth=2 " sw
autocmd! Filetype python,make set ts=4 sw=4
set expandtab

" {{{ GUI }}}
set guifont=Monospace\ 9
set guioptions-=T " disable toolbar
set guioptions-=r " disable right-hand scroll bar
set guioptions-=m " disable menu bar
set cursorline
set colorcolumn=80
set number

" {{{ Cursor }}}
set guicursor=n-v-c:block-Cursor " Force block cursor in all modes
set guicursor+=a:blinkon0 " Don't blink

" {{{ Colorscheme }}}
set t_Co=256
colorscheme PaperColor
highlight ColorColumn ctermbg=2
highlight ColorColumn guibg=Green
set hlsearch
hi Search cterm=NONE gui=NONE
hi Search ctermfg=Black ctermbg=Yellow guifg=Black guibg=Yellow

" {{{ Keybindings }}}
set pastetoggle=<F2>
" Bindings to indent via shift+tab in command mode
nmap <S-Tab> <<
" FIXME: this is broken
" Bindings to indent via shift+tab in insert mode
imap <S-Tab> <Esc> <<i
" Turn off Vim's regex handling
nnoremap / /\v
vnoremap / /\v
" Use leader (defaults to \) space to clear search results
nnoremap <leader><space> :noh<cr>
" Remap tab in normal and visual mode to jump between enclosing parens
map <tab> %
" Speed up scrolling of the viewport by moving two instead of one line
nmap <C-e> 2<C-e>
nmap <C-y> 2<C-y>
" `make' shortcuts
nmap mk :w<CR>:make!<CR><CR>
" Use ctrl-{j,k} to navigate the completion menu, and enter to confirm.
inoremap <expr> <C-j> pumvisible() ? "\<C-N>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-P>" : "\<C-k>"
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"
" NERDTree
nnoremap <leader>t :NERDTree<CR>
" YCM recompilation
nnoremap <F5> :YcmForceCompileAndDiagnostics<CR>

" Project specific settings
function! LoadCustomConfig()
  " Check for .vim.custom in the current directory.
  let l:config = ".vim.custom"
  if filereadable(l:config)
    exe "source" l:config
  endif
endfunction
autocmd! BufReadPost,BufNewFile * call LoadCustomConfig()
