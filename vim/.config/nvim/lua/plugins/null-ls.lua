local null_ls, ok = prequire("null-ls")
if not ok then
  return
end

null_ls.setup({
  sources = {
    null_ls.builtins.diagnostics.flake8,
    null_ls.builtins.formatting.black,
    null_ls.builtins.formatting.isort,
    null_ls.builtins.formatting.prettier.with({
      extra_args = { "--print-width", "79" },
    }),
    null_ls.builtins.formatting.shfmt,
    null_ls.builtins.formatting.sqlfluff,
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
