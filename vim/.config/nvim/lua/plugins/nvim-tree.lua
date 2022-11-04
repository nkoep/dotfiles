local nvim_tree, ok = prequire"nvim-tree"
if not ok then
  return
end

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

nvim_tree.setup()
