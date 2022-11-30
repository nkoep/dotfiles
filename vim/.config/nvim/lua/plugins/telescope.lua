local telescope, ok = prequire"telescope"
if not ok then
  return
end

telescope.setup {
  defaults = {
    layout_config = {
      vertical = {width = 0.9},
      horizontal = {width = 0.9}
    }
  }
}
telescope.load_extension("fzf")

local builtin = require"telescope.builtin"

map("n", "<C-p>", builtin.find_files)
map("n", "<leader>fg", builtin.live_grep)
map("n", "<leader>fb", builtin.buffers)
map("n", "<leader>fh", builtin.help_tags)
