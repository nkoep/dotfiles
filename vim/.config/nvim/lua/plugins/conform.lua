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
    json = { "jq" },
    lua = { "stylua" },
    python = function(buffer)
      -- Ruff is always available since we install the LSP via mason. For older
      -- projects that install isort and black locally, prefer those.
      if
        conform.get_formatter_info("isort", buffer).available
        and conform.get_formatter_info("black", buffer).available
      then
        return { "isort", "black" }
      else
        return { "ruff_organize_imports", "ruff_format" }
      end
    end,
    sh = { "shfmt" },
    sql = { "sqlfluff" },
    svelte = { "prettierd" },
    terraform = { "terraform_fmt" },
    typescript = { "prettierd" },
    typescriptreact = { "prettierd" },
    yaml = { "yamlfmt" },
    zsh = { "shfmt" },
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

vim.keymap.set({ "n", "v" }, "mk", function()
  local start_time = os.clock()
  if not conform.format({ async = false, timeout_ms = 5000 }) then
    vim.notify("No formatter configured")
    return
  end
  local duration_ms = 1000 * (os.clock() - start_time)
  vim.notify(string.format("Formatting took %.2f milliseconds", duration_ms))
end)
