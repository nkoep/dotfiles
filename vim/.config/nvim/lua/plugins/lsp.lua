local lspconfig, lspconfig_ok = Prequire("lspconfig")
if not lspconfig_ok then
  return
end

local mason_lspconfig, mason_lspconfig_ok = Prequire("mason-lspconfig")
if not mason_lspconfig_ok then
  return
end

local cmp_lsp, cmp_lsp_ok = Prequire("cmp_nvim_lsp")
if not cmp_lsp_ok then
  return
end

require("plugins.mason")

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

local M = {}

M.on_attach = function(client, buffer)
  -- Highlight occurrences of variable under cursor if LSP is available.
  if client.server_capabilities.documentHighlightProvider then
    vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
    vim.api.nvim_clear_autocmds({
      buffer = buffer,
      group = "lsp_document_highlight",
    })
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      callback = vim.lsp.buf.document_highlight,
      buffer = buffer,
      group = "lsp_document_highlight",
      desc = "Document Highlight",
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      callback = vim.lsp.buf.clear_references,
      buffer = buffer,
      group = "lsp_document_highlight",
      desc = "Clear All the References",
    })
  end

  -- Keybindings.
  local km = vim.keymap
  local buffer_options = { noremap = true, silent = true, buffer = buffer }
  km.set("n", "gD", vim.lsp.buf.declaration, buffer_options)
  km.set("n", "gd", vim.lsp.buf.definition, buffer_options)
  km.set("n", "K", vim.lsp.buf.hover, buffer_options)
  km.set("n", "gi", vim.lsp.buf.implementation, buffer_options)
  km.set("n", "H", vim.lsp.buf.signature_help, buffer_options)
  km.set("n", "<Space>rn", vim.lsp.buf.rename, buffer_options)
  km.set("n", "<Space>ca", vim.lsp.buf.code_action, buffer_options)
  km.set("n", "gr", vim.lsp.buf.references, buffer_options)
  km.set("n", "<Space>e", vim.diagnostic.open_float, buffer_options)
  km.set("n", "<Space>p", vim.diagnostic.goto_prev, buffer_options)
  km.set("n", "<Space>n", vim.diagnostic.goto_next, buffer_options)
  km.set("n", "<space>q", vim.diagnostic.setloclist, buffer_options)
end

local function before_init(_, config)
  local function env_venv()
    if vim.env.VIRTUAL_ENV then
      return vim.env.VIRTUAL_ENV
    end
  end

  local function rye_venv()
    local result = vim.fn.systemlist(
      "rye show | grep --color=never 'venv' | awk '{print $2}'"
    )
    if result[1] ~= "" then
      return result[1]
    end
  end

  local function poetry_venv()
    local result = vim.fn.systemlist("poetry env info --path")
    if result[1] ~= "" then
      return result[1]
    end
  end

  if config.settings.python ~= nil then
    local python_bin = vim.fn.exepath("python") or "python"
    for _, venv_source in ipairs({ env_venv, rye_venv, poetry_venv }) do
      local venv = venv_source()
      if venv ~= nil then
        python_bin = path.join(venv, "bin", "python")
        break
      end
    end
    config.settings.python.pythonPath = python_bin
  end
end

local capabilities = cmp_lsp.default_capabilities()

mason_lspconfig.setup({
  ensure_installed = {
    "bashls",
    "lua_ls",
    "pyright",
    "ruff",
    "rust_analyzer",
    "sqlls",
    "svelte",
    "terraformls",
    "ts_ls",
    "yamlls",
  },
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
        },
      },
    }
    lspconfig.lua_ls.setup(vim.tbl_extend("force", options, lua_options))
  end,
})

return M
