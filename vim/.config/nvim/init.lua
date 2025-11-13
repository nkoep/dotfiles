function Prequire(name)
  local ok, module = pcall(require, name)
  return module, ok
end

function Map(mode, lhs, rhs, silent)
  if silent == nil then
    silent = true
  end
  vim.keymap.set(mode, lhs, rhs, { silent = silent })
end

require("plugins")

require("autocmd")
require("colorscheme")
require("config")
require("keybindings")

require("plugins.cmp")
require("plugins.comment")
require("plugins.conform")
require("plugins.lsp")
require("plugins.lualine")
require("plugins.null-ls")
require("plugins.telescope")
require("plugins.treesitter")
