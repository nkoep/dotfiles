local mason, mason_ok = prequire("mason")
if not mason_ok then
  return
end

local mason_tool_installer, mason_tool_installer_ok = prequire("mason-tool-installer")
if not mason_tool_installer_ok then
  return
end

mason.setup()

mason_tool_installer.setup({
  ensure_installed = {
    "markdownlint",
    "prettierd",
    "shfmt",
    "sqlfluff",
    "stylua",
    "yamlfmt",
  },
})
