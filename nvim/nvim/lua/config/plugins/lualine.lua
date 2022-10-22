require("lualine").setup({
    options = {
        theme = "onedark",
        globalstatus = true,
        icons_enabled = true,
    },
    sections = {
        lualine_c = {
            {
                "buffers",
                mode = 2,
                symbols = {
                    modified = " +", -- Text to show when the buffer is modified
                    alternate_file = "#", -- Text to show to identify the alternate file
                    directory = "D", -- Text to show when the buffer is a directory
                },
            },
        },
    },
})
