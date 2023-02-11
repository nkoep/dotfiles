call plug#begin('~/.vim/plugged')
  " Common
  Plug 'nvim-lua/plenary.nvim'

  Plug 'NLKNguyen/papercolor-theme'
  Plug 'evanleck/vim-svelte'
  Plug 'fisadev/vim-isort'
  Plug 'hashivim/vim-terraform'
  Plug 'jamessan/vim-gnupg'
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'numToStr/Comment.nvim'
  Plug 'nvim-lualine/lualine.nvim'
  Plug 'nvim-telescope/telescope-fzf-native.nvim', {'do': 'make'}
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'psf/black', {'rev': '22.3.0'}
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'

  " LSP
  Plug 'WhoIsSethDaniel/mason-tool-installer.nvim'
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

  " null-ls
  Plug 'jose-elias-alvarez/null-ls.nvim'
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
