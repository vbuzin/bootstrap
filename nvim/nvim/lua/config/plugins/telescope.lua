require("telescope").setup({
    defaults = {
        layout_config = {
            horizontal = {
                width = { padding = 0 },
                height = { padding = 0 },
                preview_width = 0.5,
                preview_cutoff = 20,
            },
        },
    },
    extensions = {},
    mappings = {
        i = {
            ["<C-h>"] = "which_key",
            ["<C-/>"] = cd,
        },
    },
})

vim.keymap.set("n", "<leader>tC", "<cmd>Telescope commands<cr>", { silent = true })
vim.keymap.set("n", "<leader>tc", "<cmd>Telescope current_buffer_fuzzy_find<cr>", { silent = true })
vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { silent = true })
vim.keymap.set("n", "<leader>fF", "<cmd>Telescope file_browser<cr>", { silent = true })
vim.keymap.set("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { silent = true })
vim.keymap.set("n", "<leader>tm", "<cmd>Telescope marks<cr>", { silent = true })
vim.keymap.set("n", "<leader>bb", "<cmd>Telescope buffers<cr>", { silent = true })
vim.keymap.set("n", "<leader>th", "<cmd>Telescope help_tags<cr>", { silent = true })
vim.keymap.set("n", "<leader>tr", "<cmd>Telescope registers<cr>", { silent = true })
vim.keymap.set("n", "<leader>tq", "<cmd>Telescope quickfix<cr>", { silent = true })

-- load extensions
local extensions = { "file_browser" }

for _, ext in ipairs(extensions) do
    require("telescope").load_extension(ext)
end
