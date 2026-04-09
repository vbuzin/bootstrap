if vim.fn.executable("python3") ~= 1 then
	return {}
end

return {
	-- LSP: basedpyright (type checking) + ruff (lint + code actions)
	{
		"neovim/nvim-lspconfig",
		ft = { "python" },
		opts = {
			servers = {
				basedpyright = {
					settings = {
						basedpyright = {
							analysis = {
								-- "basic" avoids noise on untyped third-party code.
								-- Bump to "strict" per-project via pyrightconfig.json.
								typeCheckingMode = "basic",
								-- Honour the active venv from VIRTUAL_ENV automatically.
								autoImportCompletions = true,
							},
						},
					},
				},
				ruff = {
					-- ruff handles linting and code actions; basedpyright owns hover.
					on_init = function(client)
						client.server_capabilities.hoverProvider = false
					end,
				},
			},
		},
	},

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "python" })
			return opts
		end,
	},

	-- Formatter: ruff format + import sorting
	{
		"stevearc/conform.nvim",
		ft = { "python" },
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			opts.formatters_by_ft.python = { "ruff_organize_imports", "ruff_format" }
			return opts
		end,
	},

	-- Ensure external tools via Mason
	{
		"WhoIsSethDaniel/mason-tool-installer.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "basedpyright", "ruff", "debugpy" })
			return opts
		end,
	},

	-- DAP adapter
	{
		"jay-babu/mason-nvim-dap.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "debugpy" })
			return opts
		end,
	},

	-- nvim-dap-python: ergonomic debugpy registration + test-method debugging
	{
		"mfussenegger/nvim-dap-python",
		ft = { "python" },
		dependencies = { "mfussenegger/nvim-dap" },
		config = function()
			-- Point at the Mason-managed debugpy virtualenv
			require("dap-python").setup(
				vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python"
			)
		end,
		keys = {
			{
				"<leader>dtm",
				function()
					require("dap-python").test_method()
				end,
				ft = "python",
				desc = "Debug: Test Method",
			},
			{
				"<leader>dtc",
				function()
					require("dap-python").test_class()
				end,
				ft = "python",
				desc = "Debug: Test Class",
			},
		},
	},
}
