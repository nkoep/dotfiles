local lspconfig, ok = prequire"lspconfig"
if not ok then
  return
end
local cmp_lsp = require"cmp_nvim_lsp"
local path = require("lspconfig/util").path

local on_attach = function(client, bufnr)
  local bufopts = {noremap=true, silent=true, buffer=bufnr}

  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
  vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, bufopts)
  vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, bufopts)
  vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
  vim.keymap.set("n", "mf", function()
    vim.lsp.buf.format {
      async = false,
      timeout_ms = 2500
    }
  end, bufopts)
  vim.keymap.set("n", "<space>e", vim.diagnostic.open_float, bufopts)
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, bufopts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, bufopts)
  vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, bufopts)
end

function before_init(initialize_params, config)
  if initialize_params.clientInfo.name == "pyright" then
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
local servers = {"pyright"}
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    capabilities = capabilities,
    on_attach = on_attach,
    before_init = before_init,
  }
end
