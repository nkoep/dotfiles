require'lspconfig'.pyright.setup{}

require'nvim-treesitter.configs'.setup {
  ensure_installed = {"python", "ocaml"},
  highlight = {
    enable = true
  }
}
