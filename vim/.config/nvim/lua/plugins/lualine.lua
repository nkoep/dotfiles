local lualine, ok = prequire"lualine"
if not ok then
  return
end

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

lualine.setup {
  options = {
    theme = "dracula"
  },
  sections = {
    lualine_c = {
      "filename",
      {
        search_count,
        type = "lua_expr"
      }
    }
  }
}
