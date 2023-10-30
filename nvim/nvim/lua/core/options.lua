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

opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true

opt.cursorline = true
opt.incsearch = true
opt.number = true
opt.relativenumber = true
opt.smartcase = true
opt.smartindent = true
opt.termguicolors = true
opt.wildmode = 'longest,list,full'
opt.wildoptions = 'pum'
opt.wrap = false
