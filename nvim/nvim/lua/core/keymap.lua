local kmap, g = vim.keymap, vim.g

g.mapleader = " "

kmap.set("n", "<leader>ch", ":nohlsearch<CR>", { desc = "Clear search highlights" })

-- SECTION: Key mappings to center view after navigation
-- Search Result Navigation
kmap.set("n", "n", "nzz", { silent = true })   -- Next search result
kmap.set("n", "N", "Nzz", { silent = true })   -- Previous search result
kmap.set("n", "*", "*zz", { silent = true })   -- Search for word under cursor (forward)
kmap.set("n", "#", "#zz", { silent = true })   -- Search for word under cursor (backward)
kmap.set("n", "g*", "g*zz", { silent = true }) -- Like *, but without word boundaries

-- Jumplist Navigation (Popping Jumps)
kmap.set("n", "<C-o>", "<C-o>zz", { silent = true }) -- Go to older cursor position in jumplist
kmap.set("n", "<C-i>", "<C-i>zz", { silent = true }) -- Go to newer cursor position in jumplist (Note: <C-i> is often <Tab>)

-- Mark Navigation
kmap.set("n", "''", "''zz", { silent = true }) -- Go to position before the last jump
kmap.set("n", "``", "``zz", { silent = true }) -- Go to precise position before the last jump
kmap.set("n", "['", "['zz", { silent = true }) -- Go to the start of the previously changed/yanked text line
kmap.set("n", "]'", "]'zz", { silent = true }) -- Go to the end of the previously changed/yanked text line
kmap.set("n", "`[", "`[zz", { silent = true }) -- Go to the precise start of the previously changed/yanked text
kmap.set("n", "`]", "`]zz", { silent = true }) -- Go to the precise end of the previously changed/yanked text
kmap.set("n", "`.", "`.zz", { silent = true }) -- Go to the position of the last change in the current buffer
kmap.set("n", "'.", "'.zz", { silent = true }) -- Go to the line of the last change in the current buffer

-- Tag Stack Navigation
kmap.set("n", "<C-]>", "<C-]>zz", { silent = true }) -- Jump to the definition of the keyword under the cursor
kmap.set("n", "<C-t>", "<C-t>zz", { silent = true }) -- Jump to older tag in tag stack (pop tag)

-- Changelist Navigation
kmap.set("n", "g;", "g;zz", { silent = true }) -- Go to older position in changelist
kmap.set("n", "g,", "g,zz", { silent = true }) -- Go to newer position in changelist
-- END SECTION

-- Move selected text up and down
kmap.set("v", "J", ":m '>+1<CR>gv=gv") -- Move selection down
kmap.set("v", "K", ":m '<-2<CR>gv=gv") -- Move selection up

-- Managing buffers
kmap.set("n", "<leader>bp", ":bp<cr>")
kmap.set("n", "<leader>bn", ":bn<cr>")
kmap.set("n", "<leader>bd", ":bd<cr>")

-- Make j and k move by visual line, not actual line, when text is soft-wrapped
kmap.set("n", "j", "gj")
kmap.set("n", "k", "gk")

-- Quick-save
kmap.set("n", "<leader>w", "<cmd>w<cr>")

