local a = vim.api

-- Strip trailing whitespace on save.
a.nvim_create_autocmd({"BufWritePre"}, {
  pattern = {"*"},
  command = [[%s/\s\+$//e]],
})

-- Associate files with tex.
a.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = {"*.tex", "*.cls", "*.sty", "*.tikz"},
  callback = function()
    vim.o.filetype = "tex"
  end
})

a.nvim_create_autocmd({"WinEnter", "VimEnter"}, {
  pattern = {"*"},
  callback = function()
    vim.fn.matchadd("Todo", "TODO\\|FIXME\\|XXX\\|HACK", -1)
  end
})
