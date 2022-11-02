vim.o.shortmess = vim.o.shortmess .. "S"

local function search_count()
  if vim.api.nvim_get_vvar("hlsearch") == 1 then
    local res = vim.fn.searchcount({maxcount = 999, timeout = 500})

    if res.total > 0 then
      return string.format("%d/%d", res.current, res.total)
    end
  end

  return ""
end

require"lualine".setup {
  options = {
    theme = "dracula"
  },
  sections = {
    lualine_b = {{search_count, type = "lua_expr"}},
  },
}
