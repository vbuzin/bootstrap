require("which-key").setup({
    plugins = {
        spelling = {
            enabled = true,
            suggestions = 20,
        },
    },
    window = {
        border = "single", -- none, single, double, shadow
        winblend = 0,
    },
    layout = {
        height = { min = 5, max = 30 }, -- min and max height of the columns
    },
})
