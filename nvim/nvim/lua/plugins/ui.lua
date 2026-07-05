return {
	{ "nvim-tree/nvim-web-devicons" },
	{
		"olimorris/onedarkpro.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			require("onedarkpro").setup({
				colors = {
					onedark = {
						bg = "#080909",
						red = "#E15687",
						comment = "#7F838C",
					},
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
				theme = "onedark",
			},
		},
	},
}
