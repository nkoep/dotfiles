function map(mode, lhs, rhs, silent)
  if silent == nil then
    silent = true
  end
  vim.keymap.set(mode, lhs, rhs, {silent = silent})
end

-- Map tab to jump between enclosing parens.
map("", "<tab>", "%")

-- Use leader space to clear search results.
map("", "<leader><space>", ":noh<cr>")

-- FZF
map("n", "<C-p>", ":FZF<cr>")

-- nvim-tree
map("n", "<C-n>", ":NvimTreeToggle<cr>")

-- TODO: Migrate to vim.lsp.buf.format().
-- Run black and isort.
map("n", "mk", ":Isort<cr>:Black<cr>")