-- Map tab to jump between enclosing parens.
map("", "<Tab>", "%")

-- Use leader space to clear search results.
map("", "<Leader><Space>", ":noh<CR>")

map("n", "<C-b>", ":Git blame<CR>")
map("", "<C-h>", ":GBrowse<CR>")
