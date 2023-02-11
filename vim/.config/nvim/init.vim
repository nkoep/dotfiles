call plug#begin('~/.vim/plugged')
  Plug 'nvim-lua/plenary.nvim'

  Plug 'NLKNguyen/papercolor-theme'
  Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }
  Plug 'jamessan/vim-gnupg'
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'numToStr/Comment.nvim'
  Plug 'nvim-lualine/lualine.nvim'
  Plug 'nvim-telescope/telescope-fzf-native.nvim', {'do': 'make'}
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'

  " LSP
  Plug 'WhoIsSethDaniel/mason-tool-installer.nvim'
  Plug 'jose-elias-alvarez/null-ls.nvim'
  Plug 'neovim/nvim-lspconfig'
  Plug 'onsails/lspkind.nvim'
  Plug 'williamboman/mason-lspconfig.nvim'
  Plug 'williamboman/mason.nvim'

  " nvim-cmp
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-nvim-lua'
  Plug 'hrsh7th/cmp-nvim-lsp-signature-help'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/nvim-cmp'
call plug#end()

lua require("config")

lua require("autocmd")
lua require("keybindings")

lua require("plugins.comment")
lua require("plugins.completion")
lua require("plugins.lsp")
lua require("plugins.lualine")
lua require("plugins.null-ls")
lua require("plugins.telescope")
lua require("plugins.treesitter")
