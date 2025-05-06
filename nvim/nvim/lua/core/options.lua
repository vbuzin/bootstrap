local g, opt = vim.g, vim.opt

opt.directory = g.cache_dir .. "/swap/"
opt.undodir   = g.cache_dir .. "/undo/"
opt.backupdir = g.cache_dir .. "/backup/"

if vim.fn.executable("rg") == 1 then
  opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
  opt.grepprg = "rg --vimgrep --no-heading --smart-case"
end

opt.autowrite = true
opt.cursorline = true
opt.expandtab = true
opt.incsearch = true
opt.laststatus = 3
opt.showmode = true
opt.number = true
opt.relativenumber = true
opt.smartcase = true
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.timeout = true
opt.timeoutlen = 700
opt.undofile = true
opt.wildmode = "list:longest"
opt.wildoptions = "pum"
opt.wrap = false

-- Indentation
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2
opt.smartindent = true

