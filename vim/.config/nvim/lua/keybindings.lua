function map(mode, lhs, rhs)
  vim.keymap.set(mode, lhs, rhs, {silent = true})
end

-- Enable vim magic regex.
map("n", "/", "/\\v")
map("v", "/", "/\\v")

-- Map tab to jump between enclosing parens.
map("", "<tab>", "%")

-- Use leader space to clear search results.
map("", "<leader><space>", ":noh<cr>")

-- FZF
map("n", "<C-p>", ":FZF<cr>")

-- nvim-tree
map("n", "<C-o>", ":NvimTreeToggle<cr>")

-- TODO: Migrate to vim.lsp.buf.format().
-- Run black and isort.
map("n", "mk", ":Isort<cr>:Black<cr>")
