return {
	-- Lua LSP (lua-language-server from brew / make dev-tools)
	{
		"neovim/nvim-lspconfig",
		ft = { "lua" },
		opts = {
			servers = {
				lua_ls = {
					settings = {
						Lua = {
							diagnostics = {
								globals = { "vim" },
							},
							workspace = {
								checkThirdParty = false,
							},
							telemetry = {
								enable = false,
							},
							completion = {
								callSnippet = "Replace",
							},
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
			vim.list_extend(opts.ensure_installed, { "lua", "luadoc" })
			return opts
		end,
	},

	-- Formatter (stylua)
	{
		"stevearc/conform.nvim",
		ft = { "lua" },
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			opts.formatters_by_ft.lua = { "stylua" }
			return opts
		end,
	},
}
