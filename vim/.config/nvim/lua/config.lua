local o = vim.o
local g = vim.g
local a = vim.api

-- General settings
o.swapfile = false
o.backup = false
o.fileformat = "unix"
o.fileformats = "unix,dos"

-- Behavioral settings
o.splitright = true
o.splitbelow = true
o.wrap = false
o.scrolloff = 4
o.gdefault = true
o.foldenable = false

-- Visual settings
o.cmdheight = 2
o.number = true
o.colorcolumn = "80"
o.fillchars = "vert: "

-- Identation and tabs
o.tabstop = 2
o.shiftwidth = 2
o.expandtab = true

-- Cursor
o.cursorline = true
o.guicursor = "a:block"

-- Reverse completion menu tab cycle direction.
g.SuperTabDefaultCompletionType = "<c-n>"

-- LaTeX
g.tex_no_error = 1
g.tex_fast = ""

-- Python
g.python3_host_prog = "~/.pyenv/versions/neovim/bin/python"
