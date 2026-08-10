if vim.fn.executable("deno") ~= 1 then
	return {}
end

return {
	-- LSP: denols (ships with the deno binary)
	{
		"neovim/nvim-lspconfig",
		ft = {
			"javascript",
			"javascriptreact",
			"typescript",
			"typescriptreact",
			"json",
			"jsonc",
		},
		opts = {
			servers = {
				denols = {
					root_markers = { "deno.json", "deno.jsonc" },
					settings = {
						deno = {
							enable = true,
							lint = true,
							unstable = false,
						},
					},
				},
			},
		},
	},

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, {
				"typescript",
				"javascript",
				"tsx",
				"json",
			})
			return opts
		end,
	},

	-- Formatter: deno fmt
	{
		"stevearc/conform.nvim",
		ft = {
			"javascript",
			"javascriptreact",
			"typescript",
			"typescriptreact",
			"json",
			"jsonc",
		},
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			local deno_fmt = { "deno_fmt" }
			opts.formatters_by_ft.javascript = deno_fmt
			opts.formatters_by_ft.javascriptreact = deno_fmt
			opts.formatters_by_ft.typescript = deno_fmt
			opts.formatters_by_ft.typescriptreact = deno_fmt
			opts.formatters_by_ft.json = deno_fmt
			opts.formatters_by_ft.jsonc = deno_fmt
			return opts
		end,
	},
}
