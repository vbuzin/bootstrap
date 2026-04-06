return {
	{ "nvim-tree/nvim-web-devicons" },
	{
		"olimorris/onedarkpro.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			require("onedarkpro").setup({
				colors = {
					onedark = { bg = "#080909" },
				},
			})
			vim.cmd("colorscheme onedark")
		end,
	},
	{
		"nvim-lualine/lualine.nvim",
		event = "VeryLazy",
		opts = {
			options = {
				theme = "iceberg_dark",
			},
		},
	},
}
