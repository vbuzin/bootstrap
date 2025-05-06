return {
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require('gitsigns').setup()
    end
  },
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPre", "BufNewFile" },
    lazy = false,
    build = ":TSUpdate",
    main = "nvim-treesitter.configs",
    opts = {
      ensure_installed = { "vim", "vimdoc", "lua" },
      auto_install = true,
      highlight = { enable = true },
      incremental_selection = { enable = true },
      indent = { enable = true },
    },
  }
}
