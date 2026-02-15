return {
  {
    "neovim/nvim-lspconfig",
    ft = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
    opts = {
      servers = {
        tsserver = {
          filetypes = {
            "javascript",
            "javascriptreact",
            "javascript.jsx",
            "typescript",
            "typescriptreact",
            "typescript.tsx",
          },
        },
        tailwindcss = {
          filetypes = {
            "typescriptreact",
            "javascriptreact",
            "html",
            "css",
            "astro",
            "vue",
            "svelte",
            "eruby",
          },
          init_options = { userLanguages = { astro = "html", vue = "html", svelte = "html", eruby = "html" } },
        },
        eslint = {},
      },
    },
  },
  {
    "stevearc/conform.nvim",
    ft = { "javascript", "javascriptreact", "typescript", "typescriptreact", "css", "html", "json" },
    opts = {
      formatters_by_ft = {
        javascript = { "prettierd" },
        javascriptreact = { "prettierd" },
        typescript = { "prettierd" },
        typescriptreact = { "prettierd" },
        css = { "prettierd" },
        html = { "prettierd" },
        json = { "prettierd" },
      },
    },
  },
  {
    "jay-babu/mason-nvim-dap.nvim",
    opts = {
      ensure_installed = { "js-debug-adapter" },
    },
  },
}
