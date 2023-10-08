local g, opt = vim.g, vim.opt

opt.directory = g.cache_dir .. '/swap/'
opt.undodir   = g.cache_dir .. '/undo/'
opt.backupdir = g.cache_dir .. '/backup/'

if vim.fn.executable('rg') == 1 then
  opt.grepformat = '%f:%l:%c:%m,%f:%l:%m'
  opt.grepprg = 'rg --vimgrep --no-heading --smart-case'
end

opt.timeout = true
opt.timeoutlen = 700

opt.termguicolors = true

opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true

opt.incsearch = true
opt.smartcase = true

opt.number = true
opt.relativenumber = true

opt.cursorline = true
opt.smartindent = true
opt.wrap = false
