local colorscheme = "catppuccin"
local theme = "latte"

local catppuccin, ok = Prequire(colorscheme)
if not ok then
  vim.notify("Colorscheme " .. colorscheme .. " not found")
  return
end

catppuccin.setup({ flavour = theme, no_italic = true })
vim.cmd.colorscheme(colorscheme)
