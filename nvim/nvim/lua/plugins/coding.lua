return {
  -- Git integration
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require('gitsigns').setup()
    end
  },

  -- Treesitter for syntax highlighting and more
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPre", "BufNewFile" },
    lazy = false,
    build = ":TSUpdate",
    main = "nvim-treesitter.configs",
    opts = {
      ensure_installed = {
        "typescript", "javascript", "css", "html", "json", "toml", "vim", "vimdoc", "lua"
      },
      auto_install = true,
      highlight = { enable = true },
      incremental_selection = { enable = true },
      indent = { enable = true },
    },
  },

  -- Mason for managing LSPs, DAPs, linters, formatters
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    opts = {
      ui = {
        border = "rounded",
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗"
        }
      },
    },
  },

  -- LSP Configuration using Mason
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
    config = function()
      local lspconfig = require('lspconfig')
      local cmp_nvim_lsp = require('cmp_nvim_lsp')
      local mason_lspconfig = require("mason-lspconfig")

      -- Unified on_attach function for all LSPs
      local on_attach = function(client, bufnr)
        local opts = { noremap = true, silent = true, buffer = bufnr }
        vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<leader>ds', vim.diagnostic.open_float, opts)
        vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
        vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
      end

      -- LSP capabilities
      local capabilities = cmp_nvim_lsp.default_capabilities()
      capabilities.workspace = capabilities.workspace or {}
      capabilities.workspace.didChangeWatchedFiles = {
        dynamicRegistration = true,
      }

      -- Configure mason-lspconfig
      mason_lspconfig.setup({
        ensure_installed = {
          "ts_ls",
          "eslint",
          "html",
          "jsonls",
          "tailwindcss",
          "lua_ls",
        },
        automatic_installation = true,
        handlers = {
          function(server_name)
            lspconfig[server_name].setup({
              on_attach = on_attach,
              capabilities = capabilities,
            })
          end,
          ["lua_ls"] = function()
            lspconfig.lua_ls.setup({
              on_attach = on_attach,
              capabilities = capabilities,
              settings = {
                Lua = {
                  diagnostics = { globals = { "vim" } },
                  workspace = { checkThirdParty = false },
                  telemetry = { enable = false },
                },
              },
            })
          end,
          ["tsserver"] = function()
            lspconfig.tsserver.setup({
              on_attach = on_attach,
              capabilities = capabilities,
              filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
            })
          end,
          ["tailwindcss"] = function()
            lspconfig.tailwindcss.setup({
              on_attach = on_attach,
              capabilities = capabilities,
              filetypes = { "typescriptreact", "javascriptreact", "html", "css", "astro", "vue", "svelte" },
              init_options = { userLanguages = { astro = "html", vue = "html", svelte = "html" } },
            })
          end,
        },
      })
    end,
  },

  -- DAP (Debug Adapter Protocol) setup
  {
    "mfussenegger/nvim-dap",
    event = "VeryLazy",
    dependencies = {
      "jay-babu/mason-nvim-dap.nvim",
    },
    config = function()
      -- See nvim-dap documentation for more details on configuring adapters and launch configurations.
    end,
  },
  {
    "jay-babu/mason-nvim-dap.nvim",
    event = "VeryLazy",
    dependencies = { "williamboman/mason.nvim", "mfussenegger/nvim-dap" },
    opts = {
      ensure_installed = {
        "js-debug-adapter",
      },
      automatic_installation = false,
    },
  },
  -- Optional: DAP UI
  {
    "rcarriga/nvim-dap-ui",
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
    },
    config = function()
      require("dapui").setup()
      local dap, dapui = require("dap"), require("dapui")
      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
      dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end
    end
  },

  -- Formatting with conform.nvim
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    opts = {
      formatters_by_ft = {
        javascript = { "eslint_d" },
        javascriptreact = { "eslint_d" },
        typescript = { "eslint_d" },
        typescriptreact = { "eslint_d" },
        css = { "lsp" },
        html = { "lsp" },
        json = { "lsp" },
        lua = { "stylua" },
      },
      format_on_save = {
        timeout_ms = 500,
        lsp_fallback = true,
      },
    },
  },

  -- Autocompletion with nvim-cmp
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    opts = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      return {
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
        }),
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered()
        }
      }
    end,
    config = function(_, opts)
      require("luasnip.loaders.from_vscode").lazy_load()
      require("cmp").setup(opts)
    end,
  },
}
