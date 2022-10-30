call plug#begin('~/.vim/plugged')
  Plug 'NLKNguyen/papercolor-theme'
  Plug 'ervandew/supertab'
  Plug 'evanleck/vim-svelte'
  Plug 'fisadev/vim-isort'
  Plug 'hashivim/vim-terraform'
  Plug 'jamessan/vim-gnupg'
  Plug 'neovim/nvim-lspconfig'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'psf/black', {'rev': '22.3.0'}
  Plug 'numToStr/Comment.nvim'
  Plug 'tpope/vim-fugitive'
  Plug 'nvim-lualine/lualine.nvim'
  Plug 'nvim-tree/nvim-tree.lua'
  Plug 'kyazdani42/nvim-web-devicons'
  " {{{ neovim-cmp }}}
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/nvim-cmp'
  " vsnip for neovim-cmp
  Plug 'hrsh7th/cmp-vsnip'
  Plug 'hrsh7th/vim-vsnip'
call plug#end()

lua require("config")
lua require("keybindings")
lua require("autocmd")
lua require("plugins.comment")
lua require("plugins.completion")
lua require("plugins.lsp")
lua require("plugins.lualine")
lua require("plugins.nvim-tree")
lua require("plugins.treesitter")
