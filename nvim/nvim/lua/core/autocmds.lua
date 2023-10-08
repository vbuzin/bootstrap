local autocmd = vim.api.nvim_create_autocmd
local fn = vim.fn

-- buffer handlers
local buf_handlers_group = vim.api.nvim_create_augroup("BufHandlers", { clear = true })

autocmd({ "BufWritePre" }, { group = buf_handlers_group, pattern = "*", command = [[%s/\s\+$//e]] })
autocmd({ "BufReadPost" }, {
    group = buf_handlers_group,
    pattern = "*",
    callback = function()
        if fn.line("'\"") > 0 and fn.line("'\"") <= fn.line("$") then
            fn.setpos(".", fn.getpos("'\""))
            vim.api.nvim_feedkeys("zz", "n", true)
        end
    end,
})
