local treesitter, ok = Prequire("nvim-treesitter.configs")
if not ok then
  return
end

treesitter.setup({
  ensure_installed = {
    "bash",
    "css",
    "html",
    "javascript",
    "just",
    "lua",
    "python",
    "scss",
    "svelte",
    "terraform",
    "tsx",
    "typescript",
    "vim",
    "vimdoc",
  },
  highlight = {
    enable = true,
  },
})
