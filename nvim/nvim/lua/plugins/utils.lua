return {
  -- Snacks
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      indent = {
        enabled = true,

        indent = {
          enabled = false
        },
      },
      picker = {
        exclude = {
          ".git",
          "node_modules",
          "dist",
          "build",
          "target",
        }
      },
    },
    keys = {
      -- Files & buffers
      { "<leader>bl", function() Snacks.picker.buffers() end,                                                desc = "Buffers" },
      { "<leader>fe", function() Snacks.explorer() end,                                                      desc = "File Explorer" },
      { "<leader>ff", function() Snacks.picker.files() end,                                                  desc = "Find Files" },
      { "<leader>fr", function() Snacks.picker.recent() end,                                                 desc = "Recent" },
      { "<leader>fs", function() Snacks.picker.smart() end,                                                  desc = "Smart Find Files" },
      -- Grep
      { "<leader>sb", function() Snacks.picker.lines() end,                                                  desc = "Buffer Lines" },
      { "<leader>sg", function() Snacks.picker.grep() end,                                                   desc = "Grep" },
      { "<leader>sw", function() Snacks.picker.grep_word() end,                                              desc = "Visual selection or word", mode = { "n", "x" } },
      -- Other
      { "<leader>sC", function() Snacks.picker.commands() end,                                               desc = "Commands" },
      { "<leader>sd", function() Snacks.picker.diagnostics() end,                                            desc = "Diagnostics" },
      { "<leader>sh", function() Snacks.picker.help() end,                                                   desc = "Help Pages" },
      { "<leader>sj", function() Snacks.picker.jumps() end,                                                  desc = "Jumps" },
      { "<leader>sl", function() Snacks.picker.loclist() end,                                                desc = "Location List" },
      { "<leader>sm", function() Snacks.picker.marks() end,                                                  desc = "Marks" },
      { "<leader>sq", function() Snacks.picker.qflist() end,                                                 desc = "Quickfix List" },
      { "<leader>sr", function() Snacks.picker.registers() end,                                              desc = "Registers" },
      { "<leader>sR", function() Snacks.picker.resume() end,                                                 desc = "Resume" },
      { "<leader>su", function() Snacks.picker.undo() end,                                                   desc = "Undo History" },
      -- LSP
      { "gd",         function() Snacks.picker.lsp_definitions() end,                                        desc = "Goto Definition" },
      { "gD",         function() Snacks.picker.lsp_declarations() end,                                       desc = "Goto Declaration" },
      { "gr",         function() Snacks.picker.lsp_references() end,                                         nowait = true,                     desc = "References" },
      { "gI",         function() Snacks.picker.lsp_implementations() end,                                    desc = "Goto Implementation" },
      { "gy",         function() Snacks.picker.lsp_type_definitions() end,                                   desc = "Goto T[y]pe Definition" },
      { "<leader>ss", function() Snacks.picker.lsp_symbols() end,                                            desc = "LSP Symbols" },
      { "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end,                                  desc = "LSP Workspace Symbols" },
    }
  },
  -- Fast navigation
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      { "s",     mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc = "Flash" },
      { "S",     mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
      { "r",     mode = "o",               function() require("flash").remote() end,            desc = "Remote Flash" },
      { "R",     mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
      { "<c-s>", mode = { "c" },           function() require("flash").toggle() end,            desc = "Toggle Flash Search" },
    },
  },
  -- Which key
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      delay = 800,
    },
    keys = {
      {
        "<leader>?",
        function() require("which-key").show({ global = false }) end,
        desc = "Buffer local keymaps"
      },
    }
  },
  -- Surround
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    config = function() require("nvim-surround").setup({}) end
  },
  -- Autopair
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = true
  },
  -- Comments
  {
    "numToStr/Comment.nvim",
    event = "VeryLazy",
    config = function()
      require('Comment').setup()
    end,
  }
}
