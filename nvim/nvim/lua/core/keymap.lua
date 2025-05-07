local kmap, g = vim.keymap, vim.g

g.mapleader = " "

kmap.set("n", "<leader>ch", ":nohlsearch<CR>", { desc = "Clear search highlights" })

-- always center search results
kmap.set("n", "n", "nzz", { silent = true })
kmap.set("n", "N", "Nzz", { silent = true })
kmap.set("n", "*", "*zz", { silent = true })
kmap.set("n", "#", "#zz", { silent = true })
kmap.set("n", "g*", "g*zz", { silent = true })

-- managing buffers
kmap.set("n", "<leader>bp", ":bp<cr>")
kmap.set("n", "<leader>bn", ":bn<cr>")
kmap.set("n", "<leader>bd", ":bd<cr>")

-- make j and k move by visual line, not actual line, when text is soft-wrapped
kmap.set("n", "j", "gj")
kmap.set("n", "k", "gk")

-- quick-save
kmap.set("n", "<leader>w", "<cmd>w<cr>")