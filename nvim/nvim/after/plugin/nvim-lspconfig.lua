local kmap, api, lsp = vim.keymap, vim.api, vim.lsp
local lspconf = require('lspconfig')

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
kmap.set('n', '<space>e', vim.diagnostic.open_float)
kmap.set('n', '[d', vim.diagnostic.goto_prev)
kmap.set('n', ']d', vim.diagnostic.goto_next)
kmap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
api.nvim_create_autocmd('LspAttach', {
    group = api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- Buffer local mappings.
        -- See `:help vim.lsp.*` for documentation on any of the below functions
        local opts = { buffer = ev.buf }
        kmap.set('n', 'gD', lsp.buf.declaration, opts)
        kmap.set('n', 'gd', lsp.buf.definition, opts)
        kmap.set('n', 'K', lsp.buf.hover, opts)
        kmap.set('n', 'gi', lsp.buf.implementation, opts)
        kmap.set('n', '<C-k>', lsp.buf.signature_help, opts)
        kmap.set('n', '<space>wa', lsp.buf.add_workspace_folder, opts)
        kmap.set('n', '<space>wr', lsp.buf.remove_workspace_folder, opts)
        kmap.set('n', '<space>wl', function()
            print(vim.inspect(lsp.buf.list_workspace_folders()))
        end, opts)
        kmap.set('n', '<space>D', lsp.buf.type_definition, opts)
        kmap.set('n', '<space>rn', lsp.buf.rename, opts)
        kmap.set({ 'n', 'v' }, '<space>ca', lsp.buf.code_action, opts)
        kmap.set('n', 'gr', lsp.buf.references, opts)
        kmap.set('n', '<space>f', function()
            lsp.buf.format { async = true }
        end, opts)
    end,
})
