local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- remap space as leader key
map({ "n", "v" }, "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- buffers
map("n", "<Leader>bd", "<Cmd>silent :w <Bar> %bd <Bar> e# <Bar> bd# <CR>", {})

-- move line(s)
map("n", "<A-Up>", "<Cmd>m .-2<CR>", { noremap = false, silent = true })
map("n", "<A-Down>", "<Cmd>m .+1<CR>", { noremap = false, silent = true })
map("v", "<A-Up>", [[@='"zxk"zP`[V`]'<CR>]], opts)
map("v", "<A-Down>", [[@='"zx"zp`[V`]'<CR>]], opts)

-- tab switch buffer
map("n", "<tab>", ":bnext<CR>", opts)
map("n", "<S-tab>", ":bprev<CR>", opts)

-- cancel search highlighting with ESC
map("n", "<esc>", ":nohlsearch<Bar>:echo<CR>", opts)

-- using emacs-style keys in command and insert modes
map({ "c", "i" }, "<C-a>", "<Home>", { noremap = true }) -- start of line
map({ "c", "i" }, "<C-e>", "<End>", { noremap = true }) -- end of line
map({ "c", "i" }, "<C-d>", "<Del>", { noremap = true }) -- delete character under cursor
map({ "c", "i" }, "<C-b>", "<Left>", { noremap = true }) -- back one character
map({ "c", "i" }, "<C-f>", "<Right>", { noremap = true }) -- forward one character
map({ "c", "i" }, "<C-n>", "<Down>", { noremap = true }) -- recall newer command-line
map({ "c", "i" }, "<C-p>", "<Up>", { noremap = true }) -- recall older command-line
map({ "c", "i" }, "<A-f>", "<S-Right>", { noremap = true }) -- forward one word
map({ "c", "i" }, "<A-b>", "<S-Left>", { noremap = true }) -- backward one word
map("i", "<A-d>", "<C-o>dw", { noremap = true }) -- backward one word
map({ "c", "i" }, "<A-BS>", "<C-W>", { noremap = true }) -- backward one word
map({ "c", "i" }, "<A-Right>", "<S-Right>", { noremap = true }) -- forward one word
map({ "c", "i" }, "<A-Left>", "<S-Left>", { noremap = true }) -- backward one word
map("c", "<C-k>", "<C-\\>e(strpart(getcmdline(), 0, getcmdpos() - 1))<CR>", { noremap = true })
map("i", "<C-k>", "<C-o>D", { noremap = true })
