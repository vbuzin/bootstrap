local status_ok, leap = pcall(require, "leap")

leap.set_default_keymaps(true)

leap.setup({
    case_sensitive = false,
})

vim.cmd([[
" use clever-f
silent! unmap f
silent! unmap F
silent! unmap t
silent! unmap T
]])
