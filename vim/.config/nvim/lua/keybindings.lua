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

-- TODO: Migrate to vim.lsp.buf.format().
-- Run black and isort.
map("n", "mk", ":Isort<cr>:Black<cr>")

map("n", "<C-b>", ":Git blame<cr>")
