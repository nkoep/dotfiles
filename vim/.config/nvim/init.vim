call plug#begin('~/.vim/plugged')
  Plug 'NLKNguyen/papercolor-theme'
  Plug 'evanleck/vim-svelte'
  Plug 'fisadev/vim-isort'
  Plug 'hashivim/vim-terraform'
  Plug 'jamessan/vim-gnupg'
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'neovim/nvim-lspconfig'
  Plug 'numToStr/Comment.nvim'
  Plug 'nvim-lualine/lualine.nvim'
  Plug 'nvim-tree/nvim-tree.lua'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'psf/black', {'rev': '22.3.0'}
  Plug 'tpope/vim-fugitive'

  " nvim-cmp
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/nvim-cmp'

  " luasnip
  Plug 'L3MON4D3/LuaSnip'
  Plug 'rafamadriz/friendly-snippets'
  Plug 'saadparwaiz1/cmp_luasnip'

  " null-ls
  Plug 'jose-elias-alvarez/null-ls.nvim'
  Plug 'nvim-lua/plenary.nvim'
call plug#end()

lua require("autocmd")
lua require("config")
lua require("keybindings")
lua require("plugins.comment")
lua require("plugins.completion")
lua require("plugins.lsp")
lua require("plugins.lualine")
lua require("plugins.null-ls")
lua require("plugins.nvim-tree")
lua require("plugins.treesitter")
