local a = vim.api

-- Strip trailing whitespace on save.
a.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

-- Associate files with tex.
a.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = { "*.tex", "*.cls", "*.sty", "*.tikz" },
  callback = function()
    vim.o.filetype = "tex"
  end,
})

-- Highlight labels.
a.nvim_create_autocmd({ "WinEnter", "VimEnter" }, {
  pattern = "*",
  callback = function()
    vim.fn.matchadd("Todo", "TODO\\|FIXME\\|XXX\\|HACK", -1)
  end,
})

local plugin_config =
  vim.fn.resolve(vim.fn.expand("~") .. "/.config/nvim/lua/plugins/init.lua")
local group = a.nvim_create_augroup("install_packages", {})
a.nvim_create_autocmd("BufWritePost", {
  group = group,
  pattern = plugin_config,
  callback = function()
    vim.api.nvim_exec(
      [[
        source <afile>
        PackerClean
        PackerInstall
        PackerCompile
      ]],
      false
    )
  end,
})

-- Highlight occurrences of variable under cursor.
a.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
  pattern = "<buffer>",
  callback = function()
    vim.lsp.buf.document_highlight()
  end,
})
a.nvim_create_autocmd({ "CursorMoved" }, {
  pattern = "<buffer>",
  callback = function()
    vim.lsp.buf.clear_references()
  end,
})
