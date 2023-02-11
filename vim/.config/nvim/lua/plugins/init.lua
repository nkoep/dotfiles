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
    print("Installed packer")
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins/init.lua source <afile> | PackerSync
  augroup end
]])

local packer, ok = prequire("packer")
if not ok then
  return
end

packer.init({
  display = {
    open_fn = function()
      return require("packer.util").float({ border = "rounded" })
    end,
  },
})

return packer.startup(function(use)
  use("nvim-lua/plenary.nvim")

  use("NLKNguyen/papercolor-theme")
  use({ "iamcco/markdown-preview.nvim", run = "cd app && yarn install" })
  use("jamessan/vim-gnupg")
  use("kyazdani42/nvim-web-devicons")
  use("numToStr/Comment.nvim")
  use("nvim-lualine/lualine.nvim")
  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
  use("nvim-telescope/telescope.nvim")
  use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
  use("tpope/vim-fugitive")
  use("tpope/vim-rhubarb")

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
