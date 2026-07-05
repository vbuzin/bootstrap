local kmap, g = vim.keymap, vim.g

g.mapleader = " "

kmap.set("n", "<C-l>", "<cmd>nohlsearch<CR><cmd>redraw<CR>", { desc = "Clear highlights + redraw" })

-- SECTION: Key mappings to center view after navigation
-- Search Result Navigation
kmap.set("n", "n", "nzz", { silent = true }) -- Next search result
kmap.set("n", "N", "Nzz", { silent = true }) -- Previous search result
kmap.set("n", "*", "*zz", { silent = true }) -- Search for word under cursor (forward)
kmap.set("n", "#", "#zz", { silent = true }) -- Search for word under cursor (backward)
kmap.set("n", "g*", "g*zz", { silent = true, desc = "Word (no boundary)" }) -- Like *, but without word boundaries

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
kmap.set("n", "g;", "g;zz", { silent = true, desc = "Older change" })
kmap.set("n", "g,", "g,zz", { silent = true, desc = "Newer change" })
-- END SECTION

-- Move selected text up and down
kmap.set("v", "J", ":m '>+1<CR>gv=gv") -- Move selection down
kmap.set("v", "K", ":m '<-2<CR>gv=gv") -- Move selection up

-- Managing buffers
kmap.set("n", "<leader>bp", ":bp<cr>", { desc = "Previous" })
kmap.set("n", "<leader>bn", ":bn<cr>", { desc = "Next" })
kmap.set("n", "<leader>bd", ":bd<cr>", { desc = "Delete" })
kmap.set("n", "<leader>bk", ":bp | bd #<cr>", { desc = "Delete (keep window)" })

-- Make j and k move by visual line, not actual line, when text is soft-wrapped
kmap.set("n", "j", "gj", { desc = "Down (visual)" })
kmap.set("n", "k", "gk", { desc = "Up (visual)" })

-- Expand/shrink selection
kmap.set({ "x", "o" }, "+", function()
	require("vim.treesitter._select").select_parent(vim.v.count1)
end, { desc = "Expand selection" })

kmap.set({ "x", "o" }, "-", function()
	require("vim.treesitter._select").select_child(vim.v.count1)
end, { desc = "Shrink selection" })

-- Quick-save
kmap.set("n", "<leader>w", "<cmd>w<cr>", { desc = "Write / Save" })
