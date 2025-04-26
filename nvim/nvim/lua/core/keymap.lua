local kmap, g = vim.keymap, vim.g

g.mapleader = " "

kmap.set("n", "<leader>ch", ":nohl<CR>", { desc = "Clear search highlights" })

-- command mode
kmap.set("c", "<C-A>", "<Home>", { desc = "Start of line" })
kmap.set("c", "<C-B>", "<Left>", { desc = "Back one character" })
kmap.set("c", "<C-D>", "<Del>", { desc = "Delete character under cursor" })
kmap.set("c", "<C-E>", "<End>", { desc = "End of line" })
kmap.set("c", "<C-F>", "<Right>", { desc = "Forward one character" })
kmap.set("c", "<A-F>", "<S-Right>", { desc = "Forward one word" })
kmap.set("c", "<A-B>", "<S-Left>", { desc = "Back one word" })

-- always center search results
kmap.set("n", "n", "nzz", { silent = true })
kmap.set("n", "N", "Nzz", { silent = true })
kmap.set("n", "*", "*zz", { silent = true })
kmap.set("n", "#", "#zz", { silent = true })
kmap.set("n", "g*", "g*zz", { silent = true })

-- no arrow keys --- force yourself to use the home row
kmap.set("n", "<up>", "<nop>")
kmap.set("n", "<down>", "<nop>")
kmap.set("i", "<up>", "<nop>")
kmap.set("i", "<down>", "<nop>")
kmap.set("i", "<left>", "<nop>")
kmap.set("i", "<right>", "<nop>")

-- managing buffers
kmap.set("n", "<leader>bp", ":bp<cr>")
kmap.set("n", "<leader>bn", ":bn<cr>")
kmap.set("n", "<leader>bd", ":bd<cr>")

-- make j and k move by visual line, not actual line, when text is soft-wrapped
kmap.set("n", "j", "gj")
kmap.set("n", "k", "gk")

-- quick-save
kmap.set("n", "<leader>w", "<cmd>w<cr>")

-- turn off search highlights
kmap.set({"v", "n"}, "<Esc>", "<cmd>nohlsearch<cr>")