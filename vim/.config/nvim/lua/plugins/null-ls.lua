local null_ls, ok = Prequire("null-ls")
if not ok then
  return
end

local diagnostics = null_ls.builtins.diagnostics

null_ls.setup({
  on_attach = require("plugins.lsp").on_attach,
  sources = {
    -- TODO: Replace with https://github.com/mfussenegger/nvim-lint.
    diagnostics.markdownlint,
    diagnostics.sqlfluff.with({ extra_args = { "--dialect", "bigquery" } }),
  },
})
