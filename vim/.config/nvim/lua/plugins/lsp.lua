local _, lspconfig_ok = Prequire("lspconfig")
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

local servers = {
  "bashls",
  "lua_ls",
  "rust_analyzer",
  "sqlls",
  "svelte",
  "terraformls",
  "ts_ls",
  "ty",
  "yamlls",
}

mason_lspconfig.setup({
  ensure_installed = servers,
  automatic_enable = servers,
})

-- Configure global diagnostic settings
vim.diagnostic.config({
  -- Configure sign display
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "",
      [vim.diagnostic.severity.WARN] = "",
      [vim.diagnostic.severity.HINT] = "",
      [vim.diagnostic.severity.INFO] = "",
    },
    texthl = {
      [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
      [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
      [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
      [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
    },
    numhl = {},
  },

  float = {
    border = "rounded",
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

-- Set border for hover and signature help
vim.o.winborder = "rounded"

local M = {}

-- Define the on_attach function
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

-- Python venv detection function
local function before_init(_, config)
  local path = require("lspconfig.util").path

  local function env_venv()
    if vim.env.VIRTUAL_ENV then
      return vim.env.VIRTUAL_ENV
    end
  end

  local function poetry_venv()
    local result = vim.fn.systemlist("poetry env info --path")
    if result[1] ~= "" then
      return result[1]
    end
  end

  if config.settings and config.settings.python then
    local python_bin = vim.fn.exepath("python") or "python"
    for _, venv_source in ipairs({ env_venv, poetry_venv }) do
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

-- Configure servers using the new vim.lsp.config API
for _, server in ipairs(servers) do
  vim.lsp.config[server] = {
    capabilities = capabilities,
    on_attach = M.on_attach,
    before_init = before_init,
  }
end

vim.lsp.config["lua_ls"] = {
  capabilities = capabilities,
  on_attach = M.on_attach,
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT",
      },
      diagnostics = {
        globals = {
          "vim",
          "Prequire",
          "Map",
        },
      },
      workspace = {
        library = {
          vim.api.nvim_get_runtime_file("", true),
          vim.fn.expand("~/.config/nvim"),
        },
        checkThirdParty = false,
      },
      telemetry = {
        enable = false,
      },
    },
  },
}

return M
