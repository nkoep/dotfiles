local treesitter, ok = prequire("nvim-treesitter.configs")
if not ok then
  return
end

treesitter.setup({
  ensure_installed = {
    "bash",
    "css",
    "help",
    "html",
    "javascript",
    "lua",
    "python",
    "scss",
    "svelte",
    "tsx",
    "vim",
  },
  highlight = {
    enable = true,
  },
})
