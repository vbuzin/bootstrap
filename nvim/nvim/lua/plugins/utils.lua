return {
    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        opts = {
            delay = 800,
        },
        keys = {
            {
                "<leader>?",
                function()
                    require("which-key").show({ global = false })
                end,
                desc = "Buffer local keymaps",
            },
        },
    },
    { "kylechui/nvim-surround" },
    { "numToStr/Comment.nvim" },
    { "windwp/nvim-autopairs" },
    {
        "nvim-telescope/telescope.nvim",
        branch = "0.1.x",
        dependencies = { "nvim-lua/plenary.nvim" },
        cmd = "Telescope",
        keys = {
            { "<leader>tb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
            { "<leader>tc", "<cmd>Telescope commands<cr>", desc = "Commands" },
            { "<leader>tf", "<cmd>Telescope find_files<cr>", desc = "Find files" },
            { "<leader>tg", "<cmd>Telescope live_grep<cr>", desc = "Live grep" },
            { "<leader>th", "<cmd>Telescope help_tags<cr>", desc = "Help tags" },
            { "<leader>tj", "<cmd>Telescope jumplist<cr>", desc = "Jumplist" },
            { "<leader>tm", "<cmd>Telescope marks<cr>", desc = "Marks" },
            { "<leader>tr", "<cmd>Telescope registers<cr>", desc = "Registers" },
            { "<leader>ts", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Fuzzy search buffer" },
            { "<leader>tt", "<cmd>Telescope treesitter<cr>", desc = "Treesitter" },
        },
        config = {
            defaults = {
                layout_config = {
                    horizontal = {
                        preview_width = 0.6,
                        results_width = 0.4,
                    },
                    height = 0.96,
                    width = 0.96,
                },
            },
        },
    },
}
