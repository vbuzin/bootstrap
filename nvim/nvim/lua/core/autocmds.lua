local autocmd = vim.api.nvim_create_autocmd
local fn = vim.fn

-- buffer handlers
local buf_handlers_group = vim.api.nvim_create_augroup("BufHandlers", { clear = true })

-- Remove trailing whitespace
autocmd("BufWritePre", {
	group = buf_handlers_group,
	pattern = "*",
	callback = function()
		local save = vim.fn.winsaveview()
		vim.cmd([[%s/\s\+$//e]])
		vim.fn.winrestview(save)
	end,
})

autocmd("BufReadPost", {
	group = buf_handlers_group,
	pattern = "*",
	callback = function()
		if fn.line("'\"") > 0 and fn.line("'\"") <= fn.line("$") then
			fn.setpos(".", fn.getpos("'\""))
			vim.cmd("normal! zz")
		end
	end,
})

-- Highlight yanked text
autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
	callback = function()
		vim.hl.on_yank({ timeout = 200 })
	end,
})

-- Auto-resize splits on window resize
autocmd("VimResized", {
	group = vim.api.nvim_create_augroup("AutoResize", { clear = true }),
	callback = function()
		vim.cmd("tabdo wincmd =")
	end,
})

-- Close certain filetypes with 'q'
autocmd("FileType", {
	group = vim.api.nvim_create_augroup("CloseWithQ", { clear = true }),
	pattern = { "help", "qf", "man", "notify", "lspinfo", "checkhealth" },
	callback = function(event)
		vim.bo[event.buf].buflisted = false
		vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
	end,
})
