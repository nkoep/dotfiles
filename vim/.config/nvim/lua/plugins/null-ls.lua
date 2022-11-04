local null_ls, ok = prequire"null-ls"
if not ok then
  return
end

null_ls.setup {
  sources = {
    null_ls.builtins.formatting.isort,
    null_ls.builtins.formatting.black,
  }
}
