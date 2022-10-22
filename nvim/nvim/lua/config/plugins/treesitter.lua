local parsers = require("nvim-treesitter.parsers")

require("nvim-treesitter.configs").setup({
    ensure_installed = { "lua", "rust", "css", "html", "javascript", "json", "make", "markdown", "typescript", "yaml" },
    highlight = { enable = true },
    rainbow = { enable = true, extended_mode = true },
    textobjects = {
        select = {
            enable = true,

            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,

            keymaps = {
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["ac"] = "@class.outer",
                ["ic"] = "@class.inner",
            },
            include_surrounding_whitespace = true,
        },
    },
})
