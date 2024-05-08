local illuminate, ok = prequire("illuminate")
if not ok then
  return
end

illuminate.configure({
  delay = 0,
  providers = {
    "lsp",
    "treesitter",
  },
})
