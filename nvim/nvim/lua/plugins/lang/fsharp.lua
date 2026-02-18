-- nvim/nvim/lua/plugins/lang/fsharp.lua
return {
  -- Treesitter for F#
  {
    "nvim-treesitter/nvim-treesitter",
    ft = { "fsharp" },
    config = function()
      require("nvim-treesitter").install({ "fsharp" })
    end,
  },

  -- LSP Configuration for F# (FsAutoComplete)
  {
    "neovim/nvim-lspconfig",
    ft = { "fsharp", "fsscript", "fsx" }, -- Filetypes for F#
    opts = {
      servers = {
        fsautocomplete = {}, -- Default setup for FsAutoComplete
      },
    },
  },

  -- Formatter: FsAutoComplete provides formatting via Fantomas through LSP
  {
    "stevearc/conform.nvim",
    ft = { "fsharp" },
    opts = {
      formatters_by_ft = {
        fsharp = {},
      },
    },
  },

  -- DAP: For F#, debugging is typically done via .NET's integrated debugger.
  {
    "jay-babu/mason-nvim-dap.nvim",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "netcoredbg" })
      else
        opts.ensure_installed = { "netcoredbg" }
      end
    end,
  },
}
