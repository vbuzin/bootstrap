-- mapleader
vim.g.mapleader = " "

local opt = vim.opt -- to set options

opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true

opt.hlsearch = true
opt.incsearch = true

opt.backspace = { "indent", "eol", "start" }
opt.clipboard = "unnamedplus"
opt.completeopt = "menu,menuone,noselect"
opt.cursorline = true
opt.guicursor = ""
opt.number = true
opt.relativenumber = true
opt.termguicolors = true
opt.wrap = false
