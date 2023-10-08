local kmap, g = vim.keymap, vim.g

g.mapleader = ' '

kmap.set("n", "<leader>ch", ":nohl<CR>", { desc = "Clear search highlights" })
