function prequire(name)
  local ok, module = pcall(require, name)
  return module, ok
end

function map(mode, lhs, rhs, silent)
  if silent == nil then
    silent = true
  end
  vim.keymap.set(mode, lhs, rhs, { silent = silent })
end

require("plugins")

require("config")
require("autocmd")
require("keybindings")

require("plugins.comment")
require("plugins.completion")
require("plugins.lsp")
require("plugins.lualine")
require("plugins.null-ls")
require("plugins.telescope")
require("plugins.treesitter")
