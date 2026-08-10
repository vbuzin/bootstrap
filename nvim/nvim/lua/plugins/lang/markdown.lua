return {
	-- Treesitter (markdown + inline for proper parsing)
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "markdown", "markdown_inline" })
			return opts
		end,
	},

	-- Buffer defaults + deno fmt when available
	{
		"stevearc/conform.nvim",
		ft = { "markdown" },
		init = function()
			vim.api.nvim_create_autocmd("FileType", {
				pattern = "markdown",
				callback = function()
					vim.opt_local.wrap = true
					vim.opt_local.linebreak = true
					vim.opt_local.spell = false
				end,
			})
		end,
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			if vim.fn.executable("deno") == 1 then
				opts.formatters_by_ft.markdown = { "deno_fmt" }
			end
			return opts
		end,
	},
}
