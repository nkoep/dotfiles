local null_ls, ok = prequire("null-ls")
if not ok then
  return
end

require("mason-tool-installer").setup({
  ensure_installed = {
    -- Diagnostics
    "markdownlint",
    -- Formatting
    "black",
    "isort",
    "prettier",
    "shfmt",
    "stylua",
    "yamlfmt",
  },
})

local diagnostics = null_ls.builtins.diagnostics
local formatting = null_ls.builtins.formatting

null_ls.setup({
  on_attach = require("plugins.lsp").on_attach,
  sources = {
    -- TODO: Investigate https://github.com/mfussenegger/nvim-lint.
    -- Diagnostics
    diagnostics.markdownlint,
    -- TODO: Investigate https://github.com/stevearc/conform.nvim.
    -- Formatting
    formatting.black,
    formatting.isort,
    formatting.prettier,
    formatting.shfmt.with({
      extra_args = { "-i", "2", "-sr", "-ci", "-s" },
    }),
    formatting.stylua.with({
      extra_args = {
        "--column-width",
        "79",
        "--indent-type",
        "Spaces",
        "--indent-width",
        "2",
      },
    }),
    formatting.yamlfmt,
  },
})
