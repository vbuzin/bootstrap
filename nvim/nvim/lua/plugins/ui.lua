return {
  { "nvim-tree/nvim-web-devicons" },
  {
    "olimorris/onedarkpro.nvim",
    priority = 1000,     -- Ensure it loads first
    config = function()
      vim.cmd("colorscheme onedark")
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        theme = "auto",
      }
    },
  },
}
