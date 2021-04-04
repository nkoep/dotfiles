" Use vim rather than vi settings. This must be first, because it
" changes other options as a side effect.
set nocompatible
set noswapfile

" Set up vim-plug. Install it via:
"   curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')
Plug 'Integralist/vim-mypy'
Plug 'NLKNguyen/papercolor-theme'
Plug 'Shougo/deoplete.nvim'
Plug 'cespare/vim-toml'
Plug 'chr4/nginx.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ervandew/supertab'
Plug 'evanleck/vim-svelte'
Plug 'google/vim-searchindex'
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'psf/black'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'zchee/deoplete-jedi'
call plug#end()

" {{{ General settings }}}
set fileformat=unix
set fileformats=unix,dos
set nobackup

" Remove trailing whitespace on save.
augroup trail_save
  autocmd!
  autocmd BufWritePre * :%s/\s\+$//e
augroup END

" Add keywords TODO and FIXME to the highlight group.
autocmd! WinEnter,VimEnter *
  :silent! call matchadd('Todo', 'TODO\|FIXME\|XXX', -1)

" Associate .cls and .sty files with latex.
augroup filetypedetect
  au BufRead,BufNewFile *.tex,*.cls,*.sty,*.tikz set filetype=tex
augroup END

syntax on
syntax spell toplevel
set nofoldenable " disable code folding
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
let g:python3_host_prog = '/usr/bin/python'

let g:airline_theme = 'bubblegum'

" {{{ Indentation and tab rules }}}
set tabstop=2 " ts
set shiftwidth=2 " sw
autocmd! Filetype python set ts=4 sw=4
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
set guicursor=a:block-Cursor " Force block cursor in all modes
set guicursor+=a:blinkon0 " Don't blink

" {{{ Colorscheme }}}
set t_Co=256
set background=light
colorscheme PaperColor
highlight ColorColumn ctermbg=2
highlight ColorColumn guibg=Green
set hlsearch
hi Search gui=NONE guifg=Black guibg=Yellow
hi Search cterm=NONE ctermfg=Black ctermbg=Yellow

" {{{ Keybindings }}}
set pastetoggle=<F2>

" Turn off Vim's regex handling.
nnoremap / /\v
vnoremap / /\v

" Use leader (defaults to \) space to clear search results.
nnoremap <leader><space> :noh<cr>

" Remap tab in normal and visual mode to jump between enclosing parens.
map <tab> %

" Speed up scrolling of the viewport by moving two instead of one line at a
" time.
nmap <C-e> 2<C-e>
nmap <C-y> 2<C-y>

" black shortcut
nmap bk :Black<CR>

" Remap tag jumping to ctrl+g
nnoremap <C-g> <C-]>
command! Mktags :silent! :!ctags -R .

" Enable project specific configs (loads .vimrc files in the current directory)
set exrc
set secure

" Enable deoplete.
if has("nvim") && has("python3")
  let g:deoplete#enable_at_startup = 1
endif
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" Enable jsx syntax highlighting for regular .js files.
let g:jsx_ext_required = 0

" Reverse completion menu tab cycle direction
let g:SuperTabDefaultCompletionType = "<c-n>"

" Add svelte preprocessor highlight support for SASS
let g:svelte_preprocessors = ["sass"]

" Configure black.
let g:black_linelength = 79
