if vim.fn.executable("go") ~= 1 then
	return {}
end

return {
	-- LSP: gopls
	{
		"neovim/nvim-lspconfig",
		ft = { "go", "gomod", "gosum", "gowork" },
		opts = {
			servers = {
				gopls = {
					root_markers = { "go.mod", "go.work" },
					settings = {
						gopls = {
							gofumpt = true,
							completeUnimported = true,
							usePlaceholders = true,
							analyses = {
								unusedparams = true,
								staticcheck = true,
							},
							hints = {
								parameterNames = true,
								assignVariableTypes = false,
							},
						},
					},
				},
			},
		},
	},

	-- Treesitter: full Go ecosystem parsers
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "go", "gomod", "gosum", "gowork" })
			return opts
		end,
	},

	-- Formatter: goimports (organise imports) → gofumpt (stricter formatting)
	{
		"stevearc/conform.nvim",
		ft = { "go" },
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			opts.formatters_by_ft.go = { "goimports", "gofumpt" }
			return opts
		end,
	},

	-- Ensure external tools via Mason
	{
		"WhoIsSethDaniel/mason-tool-installer.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "gopls", "gofumpt", "goimports", "delve" })
			return opts
		end,
	},

	-- DAP adapter
	{
		"jay-babu/mason-nvim-dap.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "delve" })
			return opts
		end,
	},

	-- nvim-dap-go: ergonomic Delve registration + test debugging
	{
		"leoluz/nvim-dap-go",
		ft = { "go" },
		dependencies = { "mfussenegger/nvim-dap" },
		config = function()
			require("dap-go").setup()
		end,
		keys = {
			{
				"<leader>dt",
				function()
					require("dap-go").debug_test()
				end,
				ft = "go",
				desc = "Debug: Test",
			},
		},
	},

}
