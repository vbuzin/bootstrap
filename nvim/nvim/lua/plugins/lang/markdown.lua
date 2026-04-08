return {
	-- LSP: marksman (solid for Markdown diagnostics, links, headings)
	{
		"neovim/nvim-lspconfig",
		ft = { "markdown" },
		init = function()
			vim.api.nvim_create_autocmd("FileType", {
				pattern = "markdown",
				callback = function()
					-- sensible defaults for Markdown editing
					vim.opt_local.wrap = true
					vim.opt_local.linebreak = true
					vim.opt_local.spell = false
				end,
			})
		end,
		opts = {
			servers = {
				marksman = {},
			},
		},
	},

	-- Treesitter (markdown + inline for proper parsing)
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "markdown", "markdown_inline" })
			return opts
		end,
	},

	-- Conform: formatting with Prettier (closest equivalent to Fantomas)
	{
		"stevearc/conform.nvim",
		ft = { "markdown" },
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			opts.formatters_by_ft.markdown = { "prettierd" }
			return opts
		end,
	},
	-- Mason: install formatter
	{
		"WhoIsSethDaniel/mason-tool-installer.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "prettierd" })
			return opts
		end,
	},
	-- render-markdown.nvim with a few useful tweaks
	{
		"MeanderingProgrammer/render-markdown.nvim",
		dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
		ft = { "markdown" },
		opts = {
			-- completions work nicely with marksman
			completions = { lsp = { enabled = true } },
			-- tweak render modes or styles here if you want
		},
	},
}
