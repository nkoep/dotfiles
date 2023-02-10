local lspconfig, ok = prequire("lspconfig")
if not ok then
  return
end
local cmp_lsp = require("cmp_nvim_lsp")
local path = require("lspconfig/util").path

local signs = {
  { name = "DiagnosticSignError", text = "" },
  { name = "DiagnosticSignWarn", text = "" },
  { name = "DiagnosticSignHint", text = "" },
  { name = "DiagnosticSignInfo", text = "" },
}
for _, sign in pairs(signs) do
  vim.fn.sign_define(
    sign.name,
    { text = sign.text, texthl = sign.name, numhl = "" }
  )
end

local border = "rounded"

local handlers = {
  ["textDocument/hover"] = vim.lsp.with(
    vim.lsp.handlers.hover,
    { border = border, focusable = false }
  ),
  ["textDocument/signatureHelp"] = vim.lsp.with(
    vim.lsp.handlers.signature_help,
    { border = border }
  ),
}

vim.diagnostic.config({
  float = {
    border = border,
    focusable = false,
    header = "",
    prefix = "",
    source = "always",
    style = "minimal",
  },
  severity_sort = true,
  update_in_insert = true,
  virtual_text = false,
})

local on_attach = function(_, bufnr)
  local bufopts = { noremap = true, silent = true, buffer = bufnr }

  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
  vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set("n", "<Space>rn", vim.lsp.buf.rename, bufopts)
  vim.keymap.set("n", "<Space>ca", vim.lsp.buf.code_action, bufopts)
  vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
  vim.keymap.set("n", "mf", function()
    vim.lsp.buf.format({
      async = false,
      timeout_ms = 2500,
    })
  end, bufopts)
  vim.keymap.set("n", "<Space>e", vim.diagnostic.open_float, bufopts)
  vim.keymap.set("n", "<Space>p", vim.diagnostic.goto_prev, bufopts)
  vim.keymap.set("n", "<Space>n", vim.diagnostic.goto_next, bufopts)
  vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, bufopts)
end

local function before_init(_, config)
  if config.settings.python ~= nil then
    local python_bin
    if vim.env.VIRTUAL_ENV then
      python_bin = path.join(vim.env.VIRTUAL_ENV, "bin", "python")
    else
      python_bin = exepath("python") or "python"
    end
    config.settings.python.pythonPath = python_bin
  end
end

local capabilities = cmp_lsp.default_capabilities()

local servers = { "pyright", "eslint", "sumneko_lua", "bashls" }

require("mason").setup()

local mason_lspconfig = require("mason-lspconfig")

mason_lspconfig.setup({
  ensure_installed = servers,
})

local options = {
  capabilities = capabilities,
  on_attach = on_attach,
  before_init = before_init,
  handlers = handlers,
}

mason_lspconfig.setup_handlers({
  function(client)
    lspconfig[client].setup(options)
  end,
  ["sumneko_lua"] = function()
    local lua_options = {
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
          },
        },
      },
    }
    lspconfig.sumneko_lua.setup(vim.tbl_extend("force", options, lua_options))
  end,
})
