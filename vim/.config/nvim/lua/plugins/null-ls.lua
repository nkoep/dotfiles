local null_ls, ok = prequire("null-ls")
if not ok then
  return
end

require("mason-tool-installer").setup({
  ensure_installed = {
    -- Diagnostics
    "flake8",
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

null_ls.setup({
  on_attach = require("plugins.lsp").on_attach,
  sources = {
    -- Diagnostics
    null_ls.builtins.diagnostics.flake8,
    null_ls.builtins.diagnostics.markdownlint,
    -- Formatting
    null_ls.builtins.formatting.black,
    null_ls.builtins.formatting.isort,
    null_ls.builtins.formatting.prettier,
    null_ls.builtins.formatting.shfmt.with({
      extra_args = { "-i", "2", "-sr", "ci", "-s" },
    }),
    null_ls.builtins.formatting.stylua.with({
      extra_args = {
        "--column-width",
        "79",
        "--indent-type",
        "Spaces",
        "--indent-width",
        "2",
      },
    }),
    null_ls.builtins.formatting.yamlfmt,
  },
})
