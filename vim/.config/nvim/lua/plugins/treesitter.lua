local treesitter, ok = prequire"nvim-treesitter.configs"
if not ok then
  return
end

treesitter.setup {
  ensure_installed = {
    "bash",
    "css",
    "html",
    "javascript",
    "svelte",
    "lua",
    "python",
    "scss",
    "tsx",
  },
  highlight = {
    enable = true
  }
}
