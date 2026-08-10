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

	-- nvim-dap-python: dedicated debugpy venv from make dev-tools
	{
		"mfussenegger/nvim-dap-python",
		ft = { "python" },
		dependencies = { "mfussenegger/nvim-dap" },
		config = function()
			local python = vim.fn.stdpath("data") .. "/tools/debugpy/bin/python"
			if vim.fn.executable(python) == 1 then
				require("dap-python").setup(python)
			else
				vim.notify(
					"debugpy venv missing; run `make dev-tools`",
					vim.log.levels.WARN,
					{ title = "dap-python" }
				)
			end
		end,
		keys = {
			{
				"<leader>dtm",
				function()
					require("dap-python").test_method()
				end,
				ft = "python",
				desc = "Test Method",
			},
			{
				"<leader>dtc",
				function()
					require("dap-python").test_class()
				end,
				ft = "python",
				desc = "Test Class",
			},
		},
	},
}
