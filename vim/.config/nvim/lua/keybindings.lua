-- Map tab to jump between enclosing parens.
Map("", "<Tab>", "%")

-- Use leader space to clear search results.
Map("", "<Leader><Space>", ":noh<CR>")

Map("n", "<C-b>", ":Git blame<CR>")
Map("", "<C-h>", ":GBrowse<CR>")
