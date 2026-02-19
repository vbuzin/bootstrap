return {
  -- Add F# filetype detection
  {
    "neovim/nvim-lspconfig",
    ft = { "fsharp" },
    init = function()
      vim.filetype.add({
        extension = { fs = "fsharp", fsx = "fsharp", fsi = "fsharp" },
      })
    end,
    opts = {
      servers = {
        fsautocomplete = {
          root_markers = { ".sln", ".fsproj", ".fsx" },
        },
      },
    },
  },

  -- Install F# treesitter parser alongside the base parsers
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { "fsharp" })
      return opts
    end,
  },

  -- Conform (unchanged — LSP handles formatting)
  {
    "stevearc/conform.nvim",
    ft = { "fsharp" },
    opts = function(_, opts)
      opts.formatters = opts.formatters or {}
      opts.formatters.fantomas = {
        command = "dotnet",
        args = { "fantomas", "$FILENAME" },
        stdin = false,
      }
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.fsharp = { "fantomas" }
    end,
  },

  -- DAP
  {
    "jay-babu/mason-nvim-dap.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { "netcoredbg" })
      return opts
    end,
  },
}
