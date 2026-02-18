return {
  -- Add F# filetype detection
  {
    "neovim/nvim-lspconfig",
    ft = { "fsharp" },
    init = function()
      -- Neovim detects .fs as Forth by default; override to fsharp
      vim.filetype.add({
        extension = {
          fs = "fsharp",
          fsx = "fsharp",
          fsi = "fsharp",
        },
      })
    end,
    opts = {
      servers = {
        fsautocomplete = {},
      },
    },
    config = function(_, opts)
      -- Ensure fsautocomplete is configured and enabled when F# files load
      if opts.servers and opts.servers.fsautocomplete then
        vim.lsp.config("fsautocomplete", opts.servers.fsautocomplete)
        vim.lsp.enable("fsautocomplete")
      end
    end,
  },

  -- Install F# treesitter parser alongside the base parsers
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      -- This extends the base config rather than replacing it
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "fsharp",
        once = true,
        callback = function()
          require("nvim-treesitter").install({ "fsharp" })
        end,
      })
      return opts
    end,
  },

  -- Conform (unchanged — LSP handles formatting)
  {
    "stevearc/conform.nvim",
    ft = { "fsharp" },
    opts = {
      formatters_by_ft = {
        fsharp = {},
      },
    },
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
