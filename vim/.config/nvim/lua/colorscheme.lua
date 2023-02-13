local colorscheme = "catppuccin"
local theme = "frappe"
-- local colorscheme = "PaperColor"

if colorscheme == "catppuccin" then
  local catppuccin, ok = prequire(colorscheme)
  if not ok then
    vim.notify("Colorscheme " .. colorscheme .. " not found")
    return
  end

  catppuccin.setup({ flavour = theme, no_italic = true })
  vim.cmd.colorscheme(colorscheme)
else
  vim.o.background = "light" -- Needs to be set first.
  local ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
  if ok then
    vim.api.nvim_exec(
      [[
        hi ColorColumn ctermbg=14
        hi Search cterm=NONE ctermbg=250
        hi VertSplit cterm=NONE ctermfg=NONE ctermbg=NONE
      ]],
      false
    )
  else
    vim.notify("Colorscheme " .. colorscheme .. " not found")
  end
end
