local telescope, ok = Prequire("telescope")
if not ok then
  return
end

local builtin = require("telescope.builtin")
local actions = require("telescope.actions")

telescope.setup({
  defaults = {
    mappings = {
      i = {
        ["<Esc>"] = actions.close,
      },
    },
    layout_config = {
      vertical = { width = 0.9 },
      horizontal = { width = 0.9 },
    },
  },
})

telescope.load_extension("fzf")

Map("n", "<C-p>", builtin.find_files)
Map("n", "<C-S-p>", function()
  builtin.find_files({ hidden = true })
end)
Map("n", "<C-g>", builtin.live_grep)
