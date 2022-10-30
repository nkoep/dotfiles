require"nvim-treesitter.configs".setup {
  ensure_installed = {
    "python",
    "ocaml",
    "javascript",
    "html",
    "css",
    "scss",
    "bash",
  },
  highlight = {
    enable = true
  }
}
