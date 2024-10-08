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

  -- Visual
  use("kyazdani42/nvim-web-devicons")
  use({ "catppuccin/nvim", as = "catppuccin" })

  -- Functional
  use("elihunter173/dirbuf.nvim")
  use("jamessan/vim-gnupg")
  use("numToStr/Comment.nvim")
  use("nvim-lualine/lualine.nvim")
  use("nvim-telescope/telescope.nvim")
  use("tpope/vim-fugitive")
  use("tpope/vim-rhubarb")
  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
  use({
    "nvim-treesitter/nvim-treesitter",
    run = function()
      local ts_update =
        require("nvim-treesitter.install").update({ with_sync = true })
      ts_update()
    end,
  })

  -- LSPs, linters, formatters
  use("WhoIsSethDaniel/mason-tool-installer.nvim")
  use("neovim/nvim-lspconfig")
  use("nvimtools/none-ls.nvim")
  use("onsails/lspkind.nvim")
  use("stevearc/conform.nvim")
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
