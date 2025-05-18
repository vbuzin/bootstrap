return {
  -- Snacks
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      indent = { enabled = true },
    },
    keys = {
      -- Files & buffers
      { "<leader>bl", function() Snacks.picker.buffers() end, desc = "Buffers" },
      { "<leader>fe", function() Snacks.explorer() end, desc = "File Explorer" },
      { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files" },
      { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
      -- Grep
      { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
      { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep" },
      { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Visual selection or word", mode = { "n", "x" } },
      -- Other
      { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
      { "<leader>sh", function() Snacks.picker.help() end, desc = "Help Pages" },
      { "<leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
      { "<leader>sl", function() Snacks.picker.loclist() end, desc = "Location List" },
      { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
      { "<leader>sq", function() Snacks.picker.qflist() end, desc = "Quickfix List" },
      { "<leader>sr", function() Snacks.picker.registers() end, desc = "Registers" },
      { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume" },
      { "<leader>su", function() Snacks.picker.undo() end, desc = "Undo History" },
      -- TODOs
      { "<leader>sT", function () Snacks.picker.todo_comments({ keywords = { "TODO", "FIX", "FIXME" } }) end, desc = "Todo/Fix/Fixme" },
    }
  },
  --- Which key
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      delay = 800,
    },
    keys = {
      { "<leader>?",
        function() require("which-key").show({ global = false }) end,
        desc = "Buffer local keymaps"
      },
    }
  },
  -- Surround
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    config = function() require("nvim-surround").setup({ }) end
  },
  -- Autopair
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    config = true
  },
  {
    'windwp/nvim-ts-autotag',
    opts = {
      enable_close_on_slash = true
    }
  }
}
