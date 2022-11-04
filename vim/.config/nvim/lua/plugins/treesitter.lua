local treesitter, ok = prequire"nvim-treesitter.configs"
if not ok then
  return
end

treesitter.setup {
  ensure_installed = {
    "python",
    "ocaml",
    "javascript",
    "html",
    "css",
    "scss",
    "lua",
    "bash",
  },
  highlight = {
    enable = true
  }
}
