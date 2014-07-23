execute pathogen#infect()

" Use vim settings, rather then vi settings. This must be first, because it
" changes other options as a side effect.
set nocompatible
set noswapfile

" {{{ General settings }}}
set fileformat=unix
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
let tex_no_error=1 " turn off error highlighting in tex as it highlights
                   " underscores in non-math contexts
let g:tex_fast= ""

" {{{ Indentation and tab rules }}}
set tabstop=4
set shiftwidth=4
set expandtab

" {{{ GUI }}}
set guifont=Monospace\ 8
set guioptions-=T " disable toolbar
set guioptions-=r " disable right-hand scroll bar
set guioptions-=m " disable menu bar
set cursorline
set colorcolumn=80
set number

" {{{ Cursor }}}
set guicursor=n-v-c:block-Cursor
set guicursor+=a:blinkon0

" {{{ Colorscheme }}}
set t_Co=256
colorscheme Monokai
hi Search cterm=NONE gui=NONE
hi Search ctermfg=White ctermbg=Green guifg=White guibg=Green

" {{{ Keybindings }}}
set pastetoggle=<F2>
" shift+tab for command mode
nmap <S-Tab> <<
" shift+tab for insert mode
" FIXME: this is broken
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
nmap<C-y> 2<C-y>
" Make shortcuts
nmap mk :w<CR>:make!<CR><CR>

" Project specific settings
function! LoadCustomConfig()
    " Check for .vim.custom in the current directory.
    let l:config = getcwd() . '/.vim.custom'
    if filereadable(l:config)
        exe 'source' l:config
    endif
endfunction
autocmd! BufReadPost,BufNewFile * call LoadCustomConfig()

