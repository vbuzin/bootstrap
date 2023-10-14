local kmap, g = vim.keymap, vim.g

g.mapleader = ' '

kmap.set('n', '<leader>ch', ':nohl<CR>', { desc = 'Clear search highlights' })

-- command mode
kmap.set('c', '<C-A>', '<Home>', { desc = 'Start of line' })
kmap.set('c', '<C-B>', '<Left>', { desc = 'Back one character' })
kmap.set('c', '<C-D>', '<Del>', { desc = 'Delete character under cursor' })
kmap.set('c', '<C-E>', '<End>', { desc = 'End of line' })
kmap.set('c', '<C-F>', '<Right>', { desc = 'Forward one character' })
kmap.set('c', '<A-F>', '<S-Right>', { desc = 'Forward one word' })
kmap.set('c', '<A-B>', '<S-Left>', { desc = 'Back one word' })
