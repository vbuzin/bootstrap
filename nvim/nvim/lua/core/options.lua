local g, opt = vim.g, vim.opt

opt.directory = g.cache_dir .. "/swap/"
opt.undodir   = g.cache_dir .. "/undo/"
opt.backupdir = g.cache_dir .. "/backup/"

if vim.fn.executable("rg") == 1 then
  opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
  opt.grepprg = "rg --vimgrep --no-heading --smart-case"
end

opt.autowrite = true
opt.clipboard = "unnamedplus"
opt.cursorline = true
opt.incsearch = true
opt.laststatus = 3
opt.number = true
opt.pumheight = 15
opt.relativenumber = true
opt.scrolloff = 8
opt.showmode = false
opt.smartcase = true
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.timeout = true
opt.timeoutlen = 700
opt.undofile = true
opt.updatetime = 50
opt.wildmode = "list:longest"
opt.wildoptions = "pum"
opt.wrap = false

-- Indentation
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2
opt.smartindent = true
opt.expandtab = true

-- Project-local configuration
opt.exrc = true
opt.secure = true

