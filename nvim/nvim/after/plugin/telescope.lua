local kmap = vim.keymap

require('telescope').setup {
    defaults = {
        layout_strategy = 'vertical',
        layout_config = { 
            preview_cutoff = 1,
            width = 0.9
        }
    }
}

local builtin = require('telescope.builtin')

kmap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
kmap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
kmap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
kmap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope tags' })
kmap.set('n', '<leader>fr', builtin.registers, { desc = 'Telescope registers' })
