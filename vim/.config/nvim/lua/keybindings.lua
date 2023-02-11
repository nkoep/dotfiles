function map(mode, lhs, rhs, silent)
  if silent == nil then
    silent = true
  end
  vim.keymap.set(mode, lhs, rhs, { silent = silent })
end

-- Map tab to jump between enclosing parens.
map("", "<Tab>", "%")

-- Use leader space to clear search results.
map("", "<Leader><Space>", ":noh<CR>")

map("n", "<C-b>", ":Git blame<CR>")
map("", "<C-h>", ":GBrowse<CR>")
