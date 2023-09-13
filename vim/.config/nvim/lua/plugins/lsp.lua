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

local km = vim.keymap
local M = {}

M.on_attach = function(_, bufnr)
  local bufopts = { noremap = true, silent = true, buffer = bufnr }

  km.set("n", "gD", vim.lsp.buf.declaration, bufopts)
  km.set("n", "gd", vim.lsp.buf.definition, bufopts)
  km.set("n", "K", vim.lsp.buf.hover, bufopts)
  km.set("n", "gi", vim.lsp.buf.implementation, bufopts)
  km.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
  km.set("n", "<Space>rn", vim.lsp.buf.rename, bufopts)
  km.set("n", "<Space>ca", vim.lsp.buf.code_action, bufopts)
  km.set("n", "gr", vim.lsp.buf.references, bufopts)
  km.set("n", "mk", function()
    local start_time = os.clock()
    vim.lsp.buf.format({ async = false, timeout_ms = 2500 })
    local duration = os.clock() - start_time
    vim.notify(
      string.format("Formatting took %.2f milliseconds", duration * 1000)
    )
  end, bufopts)
  km.set("n", "<Space>e", vim.diagnostic.open_float, bufopts)
  km.set("n", "<Space>p", vim.diagnostic.goto_prev, bufopts)
  km.set("n", "<Space>n", vim.diagnostic.goto_next, bufopts)
  km.set("n", "<space>q", vim.diagnostic.setloclist, bufopts)
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

local servers = {
  "bashls",
  "hls",
  "lua_ls",
  "pyright",
  "svelte",
  "tsserver",
  "yamlls",
  "zls",
}

require("mason").setup()

local mason_lspconfig = require("mason-lspconfig")

mason_lspconfig.setup({
  ensure_installed = servers,
})

local options = {
  capabilities = capabilities,
  on_attach = M.on_attach,
  before_init = before_init,
  handlers = handlers,
}

mason_lspconfig.setup_handlers({
  function(client)
    lspconfig[client].setup(options)
  end,
  ["lua_ls"] = function()
    local lua_options = {
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
          },
          format = { enable = false },
        },
      },
    }
    lspconfig.lua_ls.setup(vim.tbl_extend("force", options, lua_options))
  end,
})

return M
