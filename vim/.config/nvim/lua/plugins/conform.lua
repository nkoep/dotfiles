local conform, ok = Prequire("conform")
if not ok then
  return
end

require("plugins.mason")

conform.setup({
  notify_on_error = true,
  default_format_opts = {
    lsp_format = "never",
  },
  formatters_by_ft = {
    javascript = { "prettierd" },
    lua = { "stylua" },
    python = { "ruff_organize_imports", "ruff_format" },
    sh = { "shfmt" },
    sql = { "sqlfluff" },
    svelte = { "prettierd" },
    terraform = { "terraform_fmt" },
    typescript = { "prettierd" },
    typescriptreact = { "prettierd" },
    yaml = { "yamlfmt" },
  },
  formatters = {
    shfmt = { prepend_args = { "-i", "2", "-ci", "-s" } },
    -- TODO(nkoep): This always fails on unfixable violations.
    sqlfluff = { args = { "fix", "--dialect", "bigquery", "-" } },
    stylua = {
      prepend_args = {
        "--column-width",
        "79",
        "--indent-type",
        "Spaces",
        "--indent-width",
        "2",
      },
    },
  },
})

vim.keymap.set({"n", "v"}, "mk", function()
  local start_time = os.clock()
  if not conform.format({ async = false, timeout_ms = 2500 }) then
    vim.notify("No formatter configured")
    return
  end
  local duration_ms = 1000 * (os.clock() - start_time)
  vim.notify(
    string.format("Formatting took %.2f milliseconds", duration_ms)
  )
end)
