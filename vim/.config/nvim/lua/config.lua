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

-- Visual settings
o.cmdheight = 2
o.number = true
o.colorcolumn = "80"
o.fillchars = "vert: "

-- Theme
o.background = "light"  -- Needs to be set first.
a.nvim_exec([[
  colorscheme PaperColor
  hi ColorColumn ctermbg=14
  hi Search cterm=NONE ctermfg=black ctermbg=yellow
  hi VertSplit cterm=NONE ctermfg=NONE ctermbg=NONE
]], false)

-- Identation and tabs
o.tabstop = 2
o.shiftwidth = 2
o.expandtab = true

-- Cursor
o.cursorline = true
o.guicursor = "a:block"

-- Reverse completion menu tab cycle direction.
g.SuperTabDefaultCompletionType = "<c-n>"

-- FZF
o.rtp = o.rtp .. "," .. "/usr/local/opt/fzf"
g.fzf_layout = {
  window = {
    width = 0.9,
    height = 0.85,
    relative = true
  }
}

-- LaTeX
g.tex_no_error = 1
g.tex_fast = ""

-- Python
-- TODO: Check if pyenv is available. If so, run it to create an nvim pyenv
--       venv, and set this variable to said venv.
g.python3_host_prog = "/usr/bin/python3"

-- Add svelte preprocessor highlight support for SASS
g.svelte_preprocessors = {"sass"}
