local ensure_packer = function()
  local fn = vim.fn

  local install_path = (
    fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
  )
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({
      "git",
      "clone",
      "--depth",
      "1",
      "https://github.com/wbthomason/packer.nvim",
      install_path,
    })
    vim.cmd([[packadd packer.nvim]])
    vim.notify("Installed packer")
    return true
  end
  return false
end

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins/init.lua source <afile> | PackerSync
  augroup end
]])

local packer_bootstrap = ensure_packer()

local packer = require("packer")

packer.init({
  display = {
    open_fn = function()
      return require("packer.util").float({ border = "rounded" })
    end,
  },
})

return packer.startup(function(use)
  use("wbthomason/packer.nvim")
  use("nvim-lua/plenary.nvim")

  use("NLKNguyen/papercolor-theme")
  use("jamessan/vim-gnupg")
  use("kyazdani42/nvim-web-devicons")
  use("numToStr/Comment.nvim")
  use("nvim-lualine/lualine.nvim")
  use("nvim-telescope/telescope.nvim")
  use("tpope/vim-fugitive")
  use("tpope/vim-rhubarb")
  use({ "iamcco/markdown-preview.nvim", run = "cd app && npm i" })
  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
  use({
    "nvim-treesitter/nvim-treesitter",
    run = function()
      local ts_update =
        require("nvim-treesitter.install").update({ with_sync = true })
      ts_update()
    end,
  })

  -- LSP
  use("WhoIsSethDaniel/mason-tool-installer.nvim")
  use("jose-elias-alvarez/null-ls.nvim")
  use("neovim/nvim-lspconfig")
  use("onsails/lspkind.nvim")
  use("williamboman/mason-lspconfig.nvim")
  use("williamboman/mason.nvim")

  -- nvim-cmp
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/cmp-cmdline")
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-nvim-lua")
  use("hrsh7th/cmp-nvim-lsp-signature-help")
  use("hrsh7th/cmp-path")
  use("hrsh7th/nvim-cmp")

  if packer_bootstrap then
    packer.sync()
  end
end)
